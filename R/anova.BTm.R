anova.BTm <- function (object, ..., dispersion = NULL, test = NULL)
{
    ## Only list models in ...
    dotargs <- list(...)
    named <- if (is.null(names(dotargs)))
        rep(FALSE, length(dotargs))
    else (names(dotargs) != "")
    if (any(named))
        warning("the following arguments to 'anova.BTm' are invalid and dropped: ",
            paste(deparse(dotargs[named]), collapse = ", "))
    dotargs <- dotargs[!named]

    ## Pass on if no random effects
    fixed <- unlist(lapply(c(list(object), dotargs),
                           function(x) is.null(x$random)))
    if (all(fixed))
        return(NextMethod())
    if (!all(!fixed))
        stop("Models must have the same random effects structure")

    ## Compare list of models (to do)
    if (length(dotargs) > 0)
        return(anova.BTmlist(c(list(object), dotargs), dispersion = dispersion,
                             test = test))

    Z <- object$random
    X <- if (n <- match("x", names(object), 0)) object[[n]]
    else model.matrix(object)
    varseq <- object$assign
    termLabels <- attr(object$terms, "term.labels") # inc sep effects

    vars <- colnames(X)
    predvars <- which(varseq != 0)
    missing <- object$missing
    sep <- factor(vars[varseq == 0], levels(object$player1[, object$id]))
    ind <- character(0)

    control <- object$control
    control$trace <- FALSE

    nvars <- max(varseq)
    stat <- df <- numeric(nvars)
    tryerror <- FALSE
    if (nvars > 1) {
        ## Extension to further methods
        method <- switch(object$method,
                         glmmPQL.fit)
        y <- object$y
        if (is.null(y)) {
            mu.eta <- object$family$mu.eta
            eta <- object$linear.predictors
            y <- object$fitted.values + object$residuals * mu.eta(eta)
        }
        for (i in 1:(nvars - 1)) {
            usex <- varseq %in% seq(i)
            if (length(missing)) {
                X1 <- missing$X1[, usex[varseq > 0], drop = FALSE]
                X1miss <- is.na(rowSums(X1))
                X2 <- missing$X2[, usex[varseq > 0], drop = FALSE]
                X2miss <- is.na(rowSums(X2))
                new.sep <- unique(unlist(list(missing$player1[X1miss],
                                              missing$player2[X2miss])))
                usex <- usex | termLabels %in% new.sep
                if (!identical(new.sep, sep)) {
                    ## replace all vars according to *current* missingness
                    ## -- may be NA for unused vars
                    X[missing$cases, predvars] <- missToZero(missing$X1, X1miss) -
                        missToZero(missing$X2, X2miss)
                    sep <- new.sep
                    fixed <- split(colnames(missing$Z),
                                   attr(missing$Z, "id") %in% sep)
                    Z[, fixed$`FALSE`] <- missing$Z[, fixed$`FALSE`]
                    Z[, fixed$`TRUE`] <- 0
                }
            }
            fit <- method(X = X[, usex, drop = FALSE], y = y, Z = Z,
                          weights = object$prior.weights,
                          offset = object$offset, family = object$family,
                          control = control,
                          sigma = object$call$sigma,
                          sigma.fixed = object$sigma.fixed)
            class(fit) <- oldClass(object)
            ind <- setdiff(vars[usex], ind)
            trystat <- try(t(coef(fit)[ind]) %*%
                           chol2inv(chol(vcov(fit, dispersion = dispersion)[ind, ind])) %*%
                           coef(fit)[ind], silent = TRUE) #vcov should deal with dispersion != 1
            if (inherits(trystat, "try-error")) {
                stat[i] <- df[i] <- NA
                tryerror <- TRUE
            }
            else {
                stat[i] <- trystat
                df[i] <- length(ind)
            }
        }
    }
    ind <- varseq == nvars | termLabels %in% setdiff(vars[varseq == 0], sep)
    trystat <- try(t(coef(object)[ind]) %*% chol2inv(chol(object$varFix[ind, ind])) %*%
                   coef(object)[ind], silent = TRUE)
    if (inherits(trystat, "try-error")) {
        stat[nvars] <- df[nvars] <- NA
        tryerror <- TRUE
    }
    else {
        stat[nvars] <- trystat
        df[nvars] <- sum(ind)
    }
    table <- data.frame(c(NA, stat), c(NA, df))
    if (length(termLabels) == 0)
        table <- table[1, , drop = FALSE]
    dimnames(table) <- list(c("NULL", termLabels[varseq > 0]), c("Statistic", "Df"))
    title <- paste("Sequential Wald Tests", "\n\nModel: ",
                   object$family$family, ", link: ", object$family$link,
                   "\n\nResponse: ", formula(terms(object))[[2]],
                   "\n\nPredictor: ", paste(formula(object), collapse = ""),
                   "\n\nTerms added sequentially (first to last)",
                   if (tryerror)
                   "\n\nTest statistic unestimable for at least one term",
                   "\n", sep = "")

    ## Assume dispersion fixed at one - if dispersion estimated, would use
    ## "residual" df from larger model in each comparison
    df.dispersion <- Inf
    if (!is.null(test)) {
        if (test == "F" && df.dispersion == Inf) {
            fam <- object$family$family
            if (fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a %s family is inappropriate",
                  fam), domain = NA)
            else warning("using F test with a fixed dispersion is inappropriate")
        }
        table <- switch(test, Chisq = {
            dfs <- table[, "Df"]
            vals <- table[, "Statistic"]
            vals[dfs %in% 0] <- NA
            cbind(table, `P(>|Chi|)` = pchisq(vals, dfs, lower.tail = FALSE))
        }, F = {
            dfs <- table[, "Df"]
            Fvalue <- table[, "Statistic"]/dfs
            Fvalue[dfs %in% 0] <- NA
            cbind(table, F = Fvalue, `Pr(>F)` =
                  pf(Fvalue, dfs, df.dispersion, lower.tail = FALSE))
        })
    }
    structure(table, heading = title, class = c("anova", "data.frame"))
}
