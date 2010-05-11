predict.glmmPQL <- function (object, newdata = NULL, newrandom = NULL, level = 1,
                             type = c("link", "response", "terms"),
                             se.fit = FALSE, dispersion = NULL, terms = NULL,
                             na.action = na.pass, ...) {
    if (object$sigma == 0 || level == 0) return(NextMethod())
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
        D <- cbind(D, newrandom)
        coef <- c(fixef(object), ranef(object))
        pred <- napredict(na.action, c(D %*% coef) + offset)
        pred <- switch(type,
                       "link" = pred,
                       "response" = family(object)$linkinv(pred),
                       "terms") ## need to fix

        if (se.fit == TRUE) {
            absorb <- function(D, c, n) {
                e1 <- D[n, ]
                e2 <- e1/c[n]
                if (n > 1) tcrossprod(e1, e2) + Recall(D, c, n - 1)
                else tcrossprod(e1, e2)
            }
            sigma <- object$sigma
            X <- model.matrix(object)
            Z <- object$random
            w <- object$weights
            c <- colSums(w * X^2)
            c <- c(c, colSums(w * Z^2) + 1/sigma^2)
            ## absorb -> diag(D %*% chol2inv(chol(C)) %*% t(D)) = var(eta)
            se.pred <- sqrt(absorb(D, length(c)))
            se.pred <- switch(type,
                              "link" = se.pred,
                              "response" = se.pred * abs(family(object)$mu.eta(pred)),
                              "terms")
            return(list(fit = pred, se.fit = se.pred))
        }

        return(pred)
    }
    else {
        ## fitted values from model plus standard errors
        ## D %*% coef + object$offset
        na.act <- object$na.action
        pred <- napredict(na.act, object$linear.predictors)
        pred <- switch(type,
                       "link" = pred,
                       "response" = family(object)$linkinv(pred),
                       "terms") ## need to fix

        if (se.fit == TRUE) {
            absorb <- function(D, C, n) {
                f <- C[n, n]
                e <- C[n, -n]
                d <- D[n, ]
                D <- D[-n, ] - tcrossprod(e/f, d)
                C <- C[-n, -n] - tcrossprod(e/f, e)
                if (n > 1) d^2/f + Recall(D, C, n - 1)
                else  d^2/f
            }
            X <- model.matrix(object)
            Z <- object$random
            D <- cbind(X, Z)
            sigma <- object$sigma
            w <- object$weights
            c <- colSums(w * X^2)
            c <- c(c, colSums(w * Z^2) + 1/sigma^2)
              wX <- sqrt(w) * X
            wZ <- sqrt(w) * Z
            XWX <- crossprod(wX)
            XWZ <- crossprod(wX, wZ)
            ZWZ <- crossprod(wZ, wZ)
            diag(ZWZ) <- diag(ZWZ) + 1/sigma^2
            C <- cbind(XWX, XWZ)
            C <- rbind(C, cbind(t(XWZ), ZWZ))
            browser()
            ## absorb -> diag(D %*% chol2inv(chol(C)) %*% t(D)) = var(eta)
            se.pred <- sqrt(absorb(t(D), C, length(c), nrow(D)))
            se.pred <- switch(type,
                              "link" = se.pred,
                              "response" = se.pred * abs(family(object)$mu.eta(pred)),
                              "terms")
            return(list(fit = pred, se.fit = se.pred))
        }

        return(pred)
    }
}
