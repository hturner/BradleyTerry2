predict.BTm <- function (object, newdata = NULL, type = c("link", "response",
    "terms"), se.fit = FALSE, dispersion = NULL, terms = NULL,
    na.action = na.pass, ...) {
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
    else
        NextMethod()
}
