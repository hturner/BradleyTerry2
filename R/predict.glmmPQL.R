predict.glmmPQL <- function (object, newdata = NULL, newrandom = NULL,
                             level = 1, type = c("link", "response", "terms"),
                             se.fit = FALSE, dispersion = NULL, terms = NULL,
                             na.action = na.pass, ...) {
    if (object$sigma == 0 || type == "terms") return(NextMethod())
    if (0 %in% level) {
        if (!is.null(newdata)) lev0 <- NextMethod()
        else lev0 <- NextMethod("predict", object, newdata = object$data)
        if (identical(level, 0)) return(lev0)
    }
    type <- match.arg(type)
    if (!is.null(newdata)) {
        ## newdata should give variables in model formula
        ## newrandom should give new design matrix for random effects
        tt <- terms(object)
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
        if (is.null(newrandom) || dim(newrandom) != c(np, ncol(object$random)))
            stop("newrandom should have ", np, " rows and ",
                 ncol(object$random), " columns")
        browser()
        D <- cbind(D, newrandom)
        coef <- c(fixef(object), ranef(object))
        pred <- napredict(na.action, c(D %*% coef) + offset)
        if (type == "response")
            pred <- family(object)$linkinv(pred)
    }
    else {
        na.action <- object$na.action
        pred <- napredict(na.action, object$linear.predictors)
        if (type == "response")
            pred <- family(object)$linkinv(pred)
    }
    if (se.fit == TRUE) {
        X <- model.matrix(object)
        Z <- object$random
        if (is.null(newdata)) D <- cbind(X, Z)
        sigma <- object$sigma
        w <- object$weights
        wX <- sqrt(w) * X
        wZ <- sqrt(w) * Z
        XWX <- crossprod(wX)
        XWZ <- crossprod(wX, wZ)
        ZWZ <- crossprod(wZ, wZ)
        diag(ZWZ) <- diag(ZWZ) + 1/sigma^2
        C <- cbind(XWX, XWZ)
        C <- rbind(C, cbind(t(XWZ), ZWZ))
        ## diag(D %*% chol2inv(chol(C)) %*% t(D)) = var(eta)
        D <- na.exclude(D)
        H <- backsolve(chol(C), t(D), transpose = TRUE)
        se.pred <- napredict(na.action,
                             napredict(attr(D, "na.action"), sqrt(colSums(H^2))))
        se.pred <- switch(type,
                          "link" = se.pred,
                          "response" = se.pred*abs(family(object)$mu.eta(pred)))
        pred <- list(fit = pred, se.fit = se.pred)
    }
    if (0 %in% level)
        list(population = lev0, individual = pred)
    else pred
}
