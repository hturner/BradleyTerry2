BTm <- function(outcome, player1, player2, formula = NULL,
                id = "..", separate.effect = NULL, refcat = NULL,
                family = binomial, data = NULL, weights = NULL, subset = NULL,
                na.action = NULL, start = NULL, etastart = NULL, mustart = NULL,
                offset = NULL, br = FALSE, sigma = 0.1, sigma.fixed = FALSE,
                control = glmmPQL.control(...), ...){
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
    model <- Diff(player1, player2, formula, id, data, separate.effect, refcat)
    mf <- cbind(Y, model$X)
    colnames(mf) <- gsub("`", "", colnames(mf))
    dummy <- as.formula(paste(deparse(substitute(outcome)), " ~ ",
                              paste(colnames(model$X), collapse = "+"),
                              " - 1", sep = ""))
    if (is.null(model$random)) {
        method <- ifelse(br, "brglm", "glm")
        fit <- do.call(method, list(dummy, family = family, data = mf,
                                    weights = weights, subset = subset,
                                    na.action = na.action, start = start,
                                    etastart = etastart, mustart = mustart,
                                    offset = model$offset, ...))
    }
    else {
        if (br)
            warning("'br' argument ignored for models with random effects")
        method <- "glmmPQL"
        method.call <- do.call(method,
                               list(fixed = dummy,
                                    random = model$random,
                                    family = family, data = mf,
                                    subset = subset, weights = weights,
                                    offset = model$offset,
                                    na.action = na.action, start = start,
                                    etastart = etastart, mustart = mustart,
                                    control = control, sigma = sigma,
                                    sigma.fixed = sigma.fixed, ...))
        fit <- eval(method.call)
    }
    fit$call <- call
    fit$id <- id
    fit$separate.effect <- separate.effect
    fit$refcat <- refcat
    fit$formula <- formula
    fit$player1 <- player1
    fit$player2 <- player2
    fit$missing <- model$missing
    fit$assign <- attr(model$X, "assign")
    fit$data <- data
    fit$random <- model$random
    class(fit) <- c("BTm", class(fit))
    fit
}
