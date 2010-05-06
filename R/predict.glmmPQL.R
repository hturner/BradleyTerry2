predict.glmmPQL <- function (object, newdata = NULL, type = c("link", "response",
    "terms"), se.fit = FALSE, dispersion = NULL, terms = NULL,
    na.action = na.pass, ...) {
    if (object$sigma == 0) return(NextMethod())
    if (!is.null(newdata)) {
        ## need to define Y and X so will work with model terms
        setup <- match(c("outcome", "player1", "player2", "formula", "id",
                        "separate.ability", "refcat", "weights",
                        "subset", "offset", "contrasts"), names(object$call), 0L)
        setup <- as.call(c(quote(BradleyTerry2:::BTm.setup),
                           as.list(object$call)[setup],
                           list(data = newdata)))
        setup <- eval(setup, environment(object$formula))
        newdata <- data.frame(X = setup$X[,1])
        newdata$X <- setup$X
        newdata$Y <- setup$Y
        return(NextMethod())
    }
    else {
        ## fitted values from model plus standard errors
        na.act <- object$na.action
        fit <- napredict(na.act, fitted(object))
        absorb <- function(D, c, n) {
            e1 <- D[n, ]
            e2 <- e1/c[n]
            if (n > 1) tcrossprod(e1, e2) + Recall(D, c, n - 1)
            else tcrossprod(e1, e2)
        }
        coef <- c(fixef(object), ranef(object)) ## need functions and ranef here
        X <- model.matrix(object)
        Z <- object$random
        D <- cbind(X, Z)
        pred <- c(D %*% coef)
        type <- match.arg(type)
        pred <- switch(type,
                       "link" = pred,
                       "response" = family(object)$linkinv(pred),
                       "terms") ## need to fix
        if (se.fit == TRUE) {
            sigma <- object$sigma
            w <- object$weights
            c <- colSums(w * X^2)
            c <- c(c, colSums(w * Z^2) + 1/sigma^2)
            browser()
            ## absorb -> D %*% chol2inv(chol(C)) %*% t(D) = -var(eta)
            se.pred <- sqrt(diag(absorb(t(D), c, length(c))))
            se.pred <- switch(type,
                              "link" = se.pred,
                              "response" = se.pred * abs(family(object)$mu.eta(pred)),
                              "terms")
            return(list(fit = pred, se.fit = se.pred))
        }
        return(pred)
    }
}
