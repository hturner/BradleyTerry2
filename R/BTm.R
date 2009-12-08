BTm <- function(outcome, player1, player2, formula = NULL,
                id = "..", separate.effect = NULL, refcat = NULL,
                family = binomial, data = NULL, weights = NULL, subset = NULL,
                na.action = NULL, start = NULL, etastart = NULL, mustart = NULL,
                offset = NULL, br = FALSE, control = glmmPQL.control(...),
                model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ...){
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

    if (!is.data.frame(data))
        data <- unlist(data, recursive = FALSE) ##-- subset etc? apply to model.frame
    ## (will take first occurence of replicated names)
    withIfNecessary <- function(x, data) {
        if (class(try(eval(x), silent = TRUE)) == "try-error")
            eval(substitute(data.frame(x), list(x = x)), data)
        else eval(substitute(data.frame(x), list(x = x)))
    }
    Y <- withIfNecessary(substitute(outcome), data)
    player1 <- withIfNecessary(substitute(player1), data)
    player2 <- withIfNecessary(substitute(player2), data)
    if (ncol(player1) == 1) colnames(player1) <- colnames(player2) <- id
    if (is.null(formula)) formula <- reformulate(id)
    diffModel <- Diff(player1, player2, formula, id, data, separate.effect, refcat,
                      contrasts)
    mf <- cbind(Y, diffModel$X)
    colnames(mf) <- gsub("`", "", colnames(mf))
    dummy <- as.formula(paste(deparse(substitute(outcome)), " ~ ",
                              paste(colnames(diffModel$X), collapse = "+"),
                              " - 1", sep = ""))
    fcall <- as.list(match.call(expand.dots = FALSE))
    argPos <- match(c("weights", "subset", "na.action", "start", "etastart",
                      "mustart", "control", "model", "x"), names(fcall), 0)
    dotArgs <- fcall$"..."
    if (is.null(diffModel$random)) {
        method <- get(ifelse(br, "brglm", "glm"), mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(formula = dummy, family = family, data = mf,
                              offset = diffModel$offset), dotArgs))
        fit <- eval(fit, parent.frame())
    }
    else {
        method <- get("glmmPQL", mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(dummy, diffModel$random, family = family,
                              data = mf, offset = diffModel$offset), dotArgs))
        fit <- eval(fit, parent.frame())
        if (!identical(fit$sigma, 0))  fit$random <- diffModel$random
        if (br) {
            if (identical(fit$sigma, 0)){
                argPos <- match(c("weights", "subset", "na.action", "model", "x"),
                                names(fcall), 0)
                fit <- as.call(c(as.name("brglm"), fcall[argPos],
                                 list(dummy, family = family, data = mf,
                                      offset = diffModel$offset,
                                      etastart = fit$linear.predictors),
                                 dotArgs))
                fit <- eval(fit, parent.frame())
            }
            else
                warning("'br' argument ignored for models with random effects",
                        call. = FALSE)
        }
    }
    fit$call <- call
    fit$id <- id
    fit$separate.effect <- separate.effect
    fit$refcat <- refcat
    fit$formula <- formula
    fit$player1 <- player1
    fit$player2 <- player2
    fit$missing <- diffModel$missing
    fit$assign <- attr(diffModel$X, "assign")
    fit$data <- data
    class(fit) <- c("BTm", class(fit))
    fit
}
