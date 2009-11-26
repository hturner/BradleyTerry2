add1.BTm <- function(object, scope, scale = 0, test = c("none", "Chisq", "F"),
                      x = NULL, ...) {
    old.form <- formula(object)
    new.form <- scope

    orandom <- lme4:::expandSlash(lme4:::findbars(old.form[[2]]))
    srandom <- lme4:::expandSlash(lme4:::findbars(new.form[[2]]))
    if (!identical(orandom, srandom))
        stop("Random effect structure of object and scope must be identical.")
    if (is.null(orandom)) return(NextMethod())

    if (!is.character(scope))
        scope <- add.scope(old.form, update.formula(old.form, scope))
    if (!length(scope))
        stop("no terms in scope for adding to object")

    # y & wt don't change as can only have NA in response
    y <- object$y
    wt <- object$prior.weights
    offset <- object$offset
    if (is.null(x)) {
        model <- Diff(object$player1, object$player2, new.form, object$id,
                      object$data, object$separate.effect, object$refcat)
        x <- model$X
        Z <- model$random
        missing <- model$missing
        if (!is.null(model$offset))
            warning("ignoring offset terms in scope")
    }
    else {
        Z <- object$random
        missing <- object$missing
    }

    ## use original term labels: no sep effects or backticks (typically)
    oTerms <- attr(terms(lme4:::nobars(old.form)), "term.labels")
    Terms <- attr(terms(lme4:::nobars(new.form)), "term.labels")
    oasgn <- object$assign
    asgn <- attr(x, "assign")
    ousex <- asgn %in% oasgn

    sTerms <- sapply(strsplit(Terms, ":", fixed = TRUE),
                     function(x) paste(sort(x), collapse = ":"))

    X <- x[, ousex, drop = FALSE]
    #n <- nrow(x)
    vars <- colnames(x)
    predvars <- which(asgn != 0)
    sep <- factor(vars[asgn == 0], levels(object$player1[, object$id]))

    method <- switch(object$method,
                     glmmPQL.fit)
    control <- object$control
    control$trace <- FALSE

    ns <- length(scope)
    stat <- df <- numeric(ns) # don't add in original as don't need for tests
    names(stat) <- names(df) <- as.character(sapply(scope, as.name))
    tryerror <- FALSE
    for (i in seq(scope)) {
        stt <- paste(sort(strsplit(scope[i], ":")[[1]]), collapse = ":")
        usex <- match(asgn, match(stt, sTerms), 0) > 0 | ousex
        if (length(missing)) {
            X1 <- missing$X1[, asgn[usex], drop = FALSE]
            X1miss <- is.na(rowSums(X1))
            X2 <- missing$X2[, asgn[usex], drop = FALSE]
            X2miss <- is.na(rowSums(X2))
            new.sep <- unique(unlist(list(missing$player1[X1miss],
                                          missing$player2[X2miss])))
            usex <- usex | vars %in% new.sep
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
        fit <- method(X = x[, usex, drop = FALSE], y = y, Z = Z, weights = wt,
                      offset = offset, family = object$family,
                      control = control,
                      sigma = object$call$sigma,
                      sigma.fixed = object$sigma.fixed)
        class(fit) <- oldClass(object)
        ind <- (usex & !ousex)[usex]
        trystat <- try(t(coef(fit)[ind]) %*%
            chol2inv(chol(vcov(fit, dispersion = scale)[ind, ind])) %*%
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
    structure(table, heading = c(title, topnote), class = c("anova",
        "data.frame"))
}
