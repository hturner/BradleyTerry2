model.frame.BTm <- function (formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("outcome", "player1", "player2",
                          "separate.ability", "refcat", "data", "weights",
                          "subset", "offset", "contrasts"), names(dots), 0L)]
    mfArgs <- dots[match(c("na.action", "start", "etastart", "mustart"),
                         names(dots), 0L)]
    if (length(nargs) || is.null(formula$model)) {
        fcall <- formula$call
        fcall[[1L]] <- as.name("BTm.model.frame")
        fcall[names(nargs)] <- nargs
        env <- environment(formula$terms)
        if (is.null(env))
            env <- parent.frame()
        diffArgs <- eval(fcall, env)
        mf <- data.frame(X = diffArgs$X[,1])
        mf$X <- diffArgs$X
        mf$Y <- diffArgs$Y
        mf <- as.call(c(model.frame, mfArgs,
                         list(formula = Y ~ X - 1, data = mf,
                              offset = diffArgs$offset,
                              subset = diffArgs$subset,
                              weights = diffArgs$weights)))
        eval(mf, parent.frame())
    }
    else formula$model
}
