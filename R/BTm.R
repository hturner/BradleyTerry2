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
    if (!is.data.frame(data))
        data <- unlist(unname(data), recursive = FALSE) ##-- subset etc? apply to model.frame
    ## (will take first occurence of replicated names)
    withIfNecessary <- function(x, data, as.data.frame = TRUE) {
        if (as.data.frame){
            ##expr <- substitute({dat <- as.data.frame(matrix(, NROW(x), 0));
            ##                    dat$var <- x;
            ##                    names(dat) <- deparse(substitute(x), 500);
            ##                    dat}, list(x = x))
            expr <- substitute(data.frame(x), list(x = x))
        }
        else expr <- x
        if (class(try(eval(x), silent = TRUE)) == "try-error")
            eval(expr, data)
        else eval(expr)
    }
    player1 <- withIfNecessary(substitute(player1), data)
    player2 <- withIfNecessary(substitute(player2), data)
    if (ncol(player1) == 1) colnames(player1) <- colnames(player2) <- id
    Y <- withIfNecessary(substitute(outcome), c(player1, player2, data))
    Yname <- deparse(substitute(outcome), 500)
    weight <- withIfNecessary(substitute(weights), data, FALSE)
    subset1 <- withIfNecessary(substitute(subset),
                               c(player1 = list(player1),
                                 player2 = list(player2), player1, data), FALSE)
    subset2 <- withIfNecessary(substitute(subset),
                               c(player1 = list(player1),
                                 player2 = list(player2), player2, data), FALSE)
    if (is.logical(subset1)) subset <- subset1 | subset2
    else subset <- c(subset1, subset2)
    if (is.null(formula)) formula <- reformulate(id)
    diffModel <- Diff(player1, player2, formula, id, data, separate.ability,
                      refcat, contrasts)
    mf <- as.data.frame(diffModel$X)
    if (ncol(Y) == 2) mf[[Yname]] <- as.matrix(Y)
    else mf[Yname] <- Y
    colnames(mf) <- gsub("`", "", colnames(mf))
    dummy <- as.formula(paste("`", Yname, "`", " ~ ",
                              paste(colnames(diffModel$X), collapse = "+"),
                              " - 1", sep = ""))
    fcall <- as.list(match.call(expand.dots = FALSE))
    argPos <- match(c("na.action", "start", "etastart",
                      "mustart", "control", "model", "x"), names(fcall), 0)
    dotArgs <- fcall$"..."
    if (is.null(diffModel$random)) {
        method <- get(ifelse(br, "brglm", "glm"), mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(formula = dummy, family = family, data = mf,
                              offset = diffModel$offset, subset = subset,
                              weights = weights), dotArgs))
        fit <- eval(fit, parent.frame())
    }
    else {
        method <- get("glmmPQL", mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(dummy, diffModel$random, family = family,
                              data = mf, offset = diffModel$offset,
                              subset = subset, weights = weights), dotArgs))
        fit <- eval(fit, parent.frame())
        if (br) {
            if (identical(fit$sigma, 0)){
                argPos <- match(c("weights", "subset", "na.action", "model", "x"),
                                names(fcall), 0)
                fit <- as.call(c(as.name("brglm"), fcall[argPos],
                                 list(dummy, family = family, data = mf,
                                      offset = diffModel$offset,
                                      etastart = fit$linear.predictors)))
                fit <- eval(fit, parent.frame())
                fit$class <- c("glmmPQL", class(fit))
            }
            else
                warning("'br' argument ignored for models with random effects",
                        call. = FALSE)
        }
    }
    fit$call <- call
    fit$id <- id
    fit$separate.ability <- separate.ability
    fit$refcat <- refcat
    fit$formula <- formula
    fit$player1 <- player1
    fit$player2 <- player2
    fit$assign <- attr(diffModel$X, "assign")
    fit$term.labels <- diffModel$term.labels
    fit$data <- data
    fit$random <- diffModel$random
    class(fit) <- c("BTm", class(fit))
    fit
}
