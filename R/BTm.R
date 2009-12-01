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
    dummy <- substitute(outcome ~ . - 1)
    dummy <-  formula(terms.formula(dummy, data = mf))

    #modelTerms <- terms(formula, data, specials = "Diff")
    #attrTerms <- attributes(modelTerms)
    #diffID <- attrTerms$specials$Diff
    #diffTerms <- rownames(attrTerms$factors)[diffID]

    #if (is.null(diffID))
    #    stop("Model specification changed with version 0.9-1. \n",
    #         "See ?BTm or vignette('BradleyTerry') for examples.")

    #modelDataCall <- as.list(match.call(expand.dots = FALSE))
    #argPos <- match(c("data", "offset"), names(modelDataCall), 0)
    #modelDataCall <- as.call(c(as.name("model.frame"), formula = modelTerms,
    #                           modelDataCall[argPos], drop.unused.levels = TRUE,
    #                           na.action = "na.pass"))
    #modelData <- eval(modelDataCall, parent.frame())

    ## Combine offsets from each Diff term, formula and argument
    #offset <- c(lapply(diffTerms, function(nm, dat)
    #                   attr(modelData[, nm], "offset"),
    #                   modelData),
    #            list(model.offset(modelData)))
    #offset <- do.call("cbind", offset)
    #if (!is.null(offset) && ncol(offset) > 1)
    #    offset <- rowSums(offset)

    ## Ignore random component of Diffs in formula offsets/model offsets
    ## & all but first Diff in formula
    #if (!length(diffTerms) || is.null(attr(modelData[[diffTerms[1]]], "random")))
    #    random <- NULL
    #else {
    #    if (length(diffTerms) > 1)
    #        warning("Only using random component from first Diff term")
    #    random <- diffTerms[1]
    #}

    ## Ideally create model frame from false formula
    ## Diff should be an operation on variables not a variable
    ## use this for now to handle subset - na.action?
    #if (!is.null(random)) {
    #    attr.random <- attributes(modelData[[random]])
    #    modelDataCall <- as.list(match.call(expand.dots = FALSE))
    #    argPos <- match(c("subset", "na.action"), names(modelDataCall), 0)
    #    modelDataCall <- as.call(c(as.name("model.frame"), formula = ~ .,
    #                               modelDataCall[argPos],
    #                               data = list(modelData)))
    #    modelData <- eval(modelDataCall)
    #    if (!missing(subset))
    #        attr.random$random <- attr.random$random[subset,]
    #    if (length(attr(modelData, "na.action")))
    #        attr.random$random <- attr.random$random[-attr(modelData, "na.action"),]
    #    attr.random[c("dim", "dimnames")] <-
    #        attributes(modelData[[random]])[c("dim", "dimnames")]
    #    attributes(modelData[[random]]) <- attr.random
    #}
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
    #if (length(diffTerms) && nchar(diffTerms) > 35){
    #    DiffCall <- match.call(Diff, attrTerms$variables[[diffID + 1]])
    #    short <- paste("Diff(", deparse(DiffCall$player1), ", ",
    #                   deparse(DiffCall$player2), ", ...)", sep = "")
    #    names(fit$coefficients) <- gsub(diffTerms, short,
    #                                    names(fit$coefficients),
    #                                    fixed = TRUE)
    #}
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
    #fit$terms <- modelTerms
    fit$method <- method
    fit$random <- model$random
    class(fit) <- c("BTm", class(fit))
    fit
}
