BTm <- function(outcome = 1, player1, player2, formula = NULL,
                id = "..", separate.ability = NULL, refcat = NULL,
                family = binomial, data = NULL, weights = NULL, subset = NULL,
                na.action = NULL, start = NULL, etastart = NULL, mustart = NULL,
                offset = NULL, br = FALSE, model = TRUE, x = FALSE,
                contrasts = NULL, ...){
    call <- match.call()

    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("`family' not recognized")
    }
    if (family$family != "binomial")
        stop("`family' must be binomial")
    if (!family$link %in% c("logit", "probit", "cauchit"))
        stop("link for binomial family must be one of \"logit\", \"probit\"",
             "or \"cauchit\"")
    fcall <- as.list(match.call(expand.dots = FALSE))
    setup <- match(c("outcome", "player1", "player2", "formula", "id",
                        "separate.ability", "refcat", "data", "weights",
                        "subset", "offset", "contrasts"), names(fcall), 0L)
    setup <- do.call(BTm.setup, fcall[setup], envir = environment(formula))
    if (setup$saturated)
        warning("Player ability saturated - equivalent to fitting ",
                "separate abilities.")
    mf <- data.frame(X = setup$X[,1])
    mf$X <- setup$X
    mf$Y <- setup$Y
    argPos <- match(c("na.action", "start", "etastart",
                      "mustart", "control", "model", "x"), names(fcall), 0)
    dotArgs <- fcall$"..."
    if (is.null(setup$random)) {
        method <- get(ifelse(br, "brglm", "glm"), mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(formula = Y ~ X - 1, family = family, data = mf,
                              offset = setup$offset, subset = setup$subset,
                              weights = setup$weights), dotArgs))
        fit <- eval(fit, parent.frame())
    }
    else {
        method <- get("glmmPQL", mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(Y ~ X - 1, setup$random, family = family,
                              data = mf, offset = setup$offset,
                              subset = setup$subset, weights = setup$weights), dotArgs))
        fit <- eval(fit, parent.frame())
        if (br) {
            if (identical(fit$sigma, 0)){
                argPos <- match(c("na.action", "model", "x"), names(fcall), 0)
                method <- get("brglm", mode = "function")
                fit <- as.call(c(method, fcall[argPos],
                                 list(Y ~ X - 1, family = family, data = mf,
                                      offset = setup$offset,
                                      subset = setup$subset,
                                      weights = setup$weights,
                                      etastart = fit$linear.predictors)))
                fit <- eval(fit, parent.frame())
                fit$class <- c("glmmPQL", class(fit))
            }
            else
                warning("'br' argument ignored for models with random effects",
                        call. = FALSE)
        }
    }
    if (ncol(setup$X) > 1)
        names(fit$coefficients) <- substring(names(fit$coefficients), 2)
    else
        names(fit$coefficients) <- colnames(setup$X)
    fit$call <- call
    fit$id <- id
    fit$separate.ability <- separate.ability
    fit$refcat <- setup$refcat
    fit$formula <- setup$formula
    fit$player1 <- setup$player1
    fit$player2 <- setup$player2
    fit$assign <- attr(setup$X, "assign")
    fit$term.labels <- setup$term.labels
    fit$data <- setup$data
    fit$random <- setup$random
    class(fit) <- c("BTm", class(fit))
    fit
}
