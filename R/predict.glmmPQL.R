predict.glmmPQL <- function (object, newdata = NULL, newrandom = NULL,
                             level = 1, type = c("link", "response", "terms"),
                             se.fit = FALSE, dispersion = NULL, terms = NULL,
                             na.action = na.pass, ...) {
    ## only pass on if a glm
    if (object$sigma == 0) return(NextMethod())
    type <- match.arg(type)
    if (!null(newdata) || type == "terms") tt <- terms(object)
    if (!is.null(newdata)) {
        ## newdata should give variables in terms formula
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        D <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        np <- nrow(D) # n predictions
        offset <- rep(0, np)
        if (!is.null(off.num <- attr(tt, "offset")))
            for (i in off.num) offset <- offset + eval(attr(tt,
                "variables")[[i + 1]], newdata)
        if (!is.null(object$call$offset))
            offset <- offset + eval(object$call$offset, newdata)
    }
    else {
        D <- model.matrix(object)
        newrandom <- object$random
        na.action <- object$na.action
    }
    if (se.fit == TRUE) {
        sigma <- object$sigma
        w <- object$weights
        wX <- sqrt(w) * D
        wZ <- sqrt(w) * newrandom
        XWX <- crossprod(wX)
        XWZ <- crossprod(wX, wZ)
        ZWZ <- crossprod(wZ, wZ)
        diag(ZWZ) <- diag(ZWZ) + 1/sigma^2
        C <- cbind(XWX, XWZ)
        C <- chol(rbind(C, cbind(t(XWZ), ZWZ)))
    }
    if (type == "terms") { # ignore level
        coef <- fixef(object)
        aa <- attr(D, "assign")
        ll <- attr(tt, "term.labels")
        hasintercept <- attr(tt, "intercept") > 0L
        if (hasintercept) {
            ll <- c("(Intercept)", ll)
            avx <- colMeans(model.matrix(object))
            termsconst <- sum(avx * coef) #NA coefs?
            D <- sweep(D, 2, avx)
        }
        pred0 <- t(rowsum(t(D %*% diag(naToZero(coef))),
                          attr(D, "assign")))
        colnames(pred0) <- ll
        if (se.fit) {
            se.pred <- pred
            ## equiv to D %*% solve(C)[sub, sub] %*% t(D)
            p <- ncol(D)
            H <- backsolve(C, diag(nrow = p), transpose = TRUE)
            for (i in seq(length.out = length(ll)))
                se.pred[, i] <- sqrt(colSums((D %*% H[,[1:p][aa == i]])^2))
        }
    }
    if (0 %in% level) {
        pred0 <- napredict(na.action, c(D %*% fixef(object)) + offset)
        if (type == "response")
            pred0 <- family(object)$linkinv(pred0)
        if (se.fit == TRUE) {
            D <- na.exclude(D)
            ## get submatrix of full C^(-1)
            p <- ncol(D)
            #H <- backsolve(C, diag(nrow = p), transpose = TRUE)
            #se.pred[, i] <- sqrt(colSums((D %*% H[,[1:p]])^2))
            se.pred0 <- sqrt(diag(D %*% tcrossprod(chol2inv(C)[1:p, 1:p], D)))
            se.pred0 <- napredict(na.action,
                                  napredict(attr(D, "na.action"), se.pred0))
           if (type == "response")
               se.pred0 <- se.pred0*abs(family(object)$mu.eta(pred)))
            pred0 <- list(fit = pred0, se.fit = se.pred0)
        }
    }
    if (1 %in% level) {
        ## newrandom should give new design matrix for original random effects
        if (is.null(newrandom) || dim(newrandom) != c(np, ncol(object$random)))
            stop("newrandom should have ", np, " rows and ",
                 ncol(object$random), " columns")
        D <- cbind(D, newrandom)
        coef <- c(fixef(object), ranef(object))
            pred1 <- napredict(na.action, c(D %*% coef) + offset)
            if (type == "response")
                pred1 <- family(object)$linkinv(pred1)
        }
      if (se.fit == TRUE) {
        ## diag(D %*% chol2inv(C) %*% t(D)) = var(eta)
        D <- na.exclude(D)
        H <- backsolve(C, t(D), transpose = TRUE)
        se.pred <- napredict(na.action,
                             napredict(attr(D, "na.action"), sqrt(colSums(H^2))))
        if (type == "response")
            se.pred <- se.pred*abs(family(object)$mu.eta(pred)))
        pred <- list(fit = pred, se.fit = se.pred)
    }

    if (0 %in% level)
        list(population = lev0, individual = pred)
    else pred
}
