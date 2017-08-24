#' Add or Drop Single Terms to/from a Bradley Terry Model
#' 
#' Add or drop single terms within the limit specified by the `scope`
#' argument. For models with no random effects, compute an analysis of deviance
#' table, otherwise compute the Wald statistic of the parameters that have been
#' added to or dropped from the model.
#' 
#' The hierarchy is respected when considering terms to be added or dropped:
#' all main effects contained in a second-order interaction must remain, and so
#' on.
#' 
#' In a scope formula \samp{.} means \sQuote{what is already there}.
#' 
#' For `drop1`, a missing `scope` is taken to mean that all terms in
#' the model may be considered for dropping.
#' 
#' If `scope` includes player covariates and there are players with
#' missing values over these covariates, then a separate ability will be
#' estimated for these players in *all* fitted models. Similarly if there
#' are missing values in any contest-level variables in `scope`, the
#' corresponding contests will be omitted from all models.
#' 
#' If `formula` includes random effects, the same random effects structure
#' will apply to all models.
#' 
#' @aliases add1.BTm drop1.BTm
#' @param object a fitted object of class inheriting from `"BTm"`.
#' @param scope a formula specifying the model including all terms to be
#' considered for adding or dropping.
#' @param scale an estimate of the dispersion. Not implemented for models with
#' random effects.
#' @param test should a p-value be returned? The F test is only appropriate for
#' models with no random effects for which the dispersion has been estimated.
#' The Chisq test is a likelihood ratio test for models with no random effects,
#' otherwise a Wald test.
#' @param x a model matrix containing columns for all terms in the scope.
#' Useful if `add1` is to be called repeatedly.  **Warning:** no checks
#' are done on its validity.
#' @param \dots further arguments passed to [add1.glm()].
#' @return An object of class `"anova"` summarizing the differences in fit
#' between the models.
#' @author Heather Turner
#' @seealso [BTm()], [anova.BTm()]
#' @keywords models
#' @examples
#' 
#' attach(flatlizards)
#' result <- rep(1, nrow(contests))
#' BTmodel1 <- BTm(result, winner, loser,
#'                 ~ throat.PC1[..] + throat.PC3[..] + (1|..),
#'                 data = list(contests, predictors),
#'                 tol = 1e-4, sigma = 2, trace = TRUE)
#' 
#' drop1(BTmodel1)
#' 
#' add1(BTmodel1, ~ . + head.length[..] + SVL[..], test = "Chisq")
#' 
#' BTmodel2 <- update(BTmodel1, formula = ~ . + head.length[..])
#' 
#' drop1(BTmodel2, test = "Chisq")
#' 
#' @importFrom lme4 findbars nobars
#' @export
add1.BTm <- function(object, scope, scale = 0, test = c("none", "Chisq", "F"),
                      x = NULL, ...) {
    old.form <- formula(object)
    new.form <- update.formula(old.form, scope)

    if (!is.character(scope)){
        orandom <- findbars(old.form[[2]])
        srandom <- findbars(new.form[[2]])
        if (length(srandom) && !identical(orandom, srandom))
            stop("Random effects structure of object and scope must be identical.")
        scope <- add.scope(old.form, new.form)
    }
    if (!length(scope))
        stop("no terms in scope for adding to object")

    if (is.null(x)) { # create model.matrix for maximum scope
        model <- Diff(object$player1, object$player2, new.form, object$id,
                      object$data, object$separate.ability, object$refcat)
        if (sum(model$offset) > 0)
            warning("ignoring offset terms in scope")
        x <- model$X
        asgn <- attr(x, "assign")
        ## add dummy term for any separate effects
        oTerms <- c("sep"[0 %in% asgn], object$term.labels)
        object$terms <- terms(reformulate(oTerms))
        y <- object$y
        dummy <- y ~ x - 1
        if (!is.null(model$random)) {
            dummy <- update(dummy, .~ . + Z)
            Z <- model$random
        }
        argPos <- match(c("weights", "subset", "na.action"),
                        names(object$call), 0)
        mf <- as.call(c(model.frame, as.list(object$call)[argPos],
                       list(formula = dummy, offset = object$offset)))
        mf <- eval(mf, parent.frame())
        x <- mf$x
        y <- model.response(mf)
        Z <- mf$Z
        wt <- model.weights(mf)
        if (is.null(wt)) wt <- rep.int(1, length(y))
        offset <- model.offset(mf)
    }
    else {
        asgn <- attr(x, "assign")
        y <- object$y
        wt <- object$prior.weights
        offset <- object$offset
        Z <- object$random
    }

    if (is.null(object$random)){
        attr(x, "assign") <- asgn + 1
        object$formula <- formula(object$terms)
        object$x <- x
        object$y <- y
        object$random <- Z
        object$prior.weights <- wt
        object$offset <- offset
        stat.table <- NextMethod(x = x)
        attr(stat.table, "heading")[3] <- deparse(old.form)
        if (newsep <- sum(asgn == 0) - sum(object$assign ==0))
            attr(stat.table, "heading") <- c(attr(stat.table, "heading"),
                                             paste("\n", newsep,
                                                   " separate effects added\n",
                                                   sep = ""))
        attr(stat.table, "separate.abilities") <- colnames(x)[asgn == 0]
        return(stat.table)
    }

    ## use original term labels: no sep effects or backticks (typically)
    oTerms <- attr(terms(nobars(old.form)), "term.labels")
    Terms <- attr(terms(nobars(new.form)), "term.labels")
    ousex <- asgn %in% c(0, which(Terms %in% oTerms))

    sTerms <- sapply(strsplit(Terms, ":", fixed = TRUE),
                     function(x) paste(sort(x), collapse = ":"))

    method <- switch(object$method,
                     glmmPQL.fit)
    control <- object$control
    control$trace <- FALSE

    if (scale == 0) dispersion <- 1
    else dispersion <- scale

    ns <- length(scope)
    stat <- df <- numeric(ns) # don't add in original as don't need for tests
    names(stat) <- names(df) <- as.character(scope)
    tryerror <- FALSE
    for (i in seq(scope)) {
        stt <- paste(sort(strsplit(scope[i], ":")[[1]]), collapse = ":")
        usex <- match(asgn, match(stt, sTerms), 0) > 0 | ousex
        fit <- method(X = x[, usex, drop = FALSE], y = y, Z = Z, weights = wt,
                      offset = offset, family = object$family,
                      control = control,
                      sigma = object$call$sigma,
                      sigma.fixed = object$sigma.fixed)
        class(fit) <- oldClass(object)
        ind <- (usex & !ousex)[usex]
        trystat <- try(t(coef(fit)[ind]) %*%
            chol2inv(chol(vcov(fit, dispersion = dispersion)[ind, ind])) %*%
                coef(fit)[ind], silent = TRUE) #vcov should deal with dispersion != 1
        if (inherits(trystat, "try-error")) {
            stat[i] <- df[i] <- NA
            tryerror <- TRUE
        }
        else {
            stat[i] <- trystat
            df[i] <- sum(ind)
        }
    }
    table <- data.frame(stat, df)
    dimnames(table) <- list(names(df), c("Statistic", "Df"))
    title <- "Single term additions\n"
    topnote <- paste("Model: ", deparse(as.vector(formula(object))),
                     if (scale > 0) paste("\nscale: ", format(scale), "\n"),
                     if (tryerror)
                     "\n\nTest statistic unestimable for at least one term")
    test <- match.arg(test)
    if (test == "Chisq") {
        dfs <- table[, "Df"]
        vals <- table[, "Statistic"]
        vals[dfs %in% 0] <- NA
        table <- cbind(table, `P(>|Chi|)` = pchisq(vals, abs(dfs),
                              lower.tail = FALSE))
    }
    else if (test == "F") {
        ## Assume dispersion fixed at one - if dispersion estimated, would use
        ## "residual" df from larger model in each comparison
        df.dispersion <- Inf
        if (df.dispersion == Inf) {
            fam <- object[[1]]$family$family
            if (fam == "binomial" || fam == "poisson")
                warning(gettextf("using F test with a '%s' family is inappropriate",
                                 fam), domain = NA, call. = FALSE)
            else warning("using F test with a fixed dispersion is inappropriate")
        }
        dfs <- table[, "Df"]
        Fvalue <- table[, "Statistic"]/abs(dfs)
        Fvalue[dfs %in% 0] <- NA
        table <- cbind(table, F = Fvalue, `Pr(>F)` =
                       pf(Fvalue, abs(dfs), df.dispersion,
                          lower.tail = FALSE))
    }
    if (newsep <- sum(asgn == 0) - sum(object$assign ==0))
        heading <- c(heading, paste("\n", newsep,
                                    " separate effects added\n",
                                    sep = ""))
    structure(table, heading = c(title, topnote),
              class = c("anova", "data.frame"),
              separate.abilities = colnames(x)[asgn == 0])
}
