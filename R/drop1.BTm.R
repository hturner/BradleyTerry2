drop1.BTm <- function(object, scope, scale = 0, test = c("none", "Chisq", "F"),
                      ...) {
    ## Pass on if no random effects
    if (is.null(object$random)){
        object$formula <- formula(terms(object))
        return(NextMethod())
    }

    form <- formula(object)

    if (missing(scope))
        scope <- drop.scope(lme4:::nobars(form))
     else {
        if (!is.character(scope)) {
            srandom <- lme4:::expandSlash(lme4:::findbars(scope[[2]]))
            if (length(srandom))
                stop("Scope should not include random effects.")
            scope <- attr(terms(update.formula(form, scope)),
                          "term.labels")
        }
        if (!all(match(scope, terms(form), 0L) > 0L))
            stop("scope is not a subset of term labels")
    }

    x <- model.matrix(object)
    asgn <- object$assign

    missing <- object$missing
    vars <- colnames(x)
    sep <- factor(vars[asgn == 0], levels(object$player1[, object$id]))

    coefs <- coef(object)
    vc <- vcov(object, dispersion = scale) #vcov should deal with dispersion != 1

    sTerms <- sapply(strsplit(scope, ":", fixed = TRUE),
                     function(x) paste(sort(x), collapse = ":"))
    stat <- df <- numeric(length(scope))
    names(stat) <- names(df) <- as.character(sapply(scope, as.name))
    tryerror <- FALSE
    for (i in seq(scope)) {
        stt <- paste(sort(strsplit(scope[i], ":")[[1]]), collapse = ":")
        usex <- match(asgn, match(stt, sTerms), 0) > 0
        if (length(missing)) {
            X1 <- missing$X1[, !usex[asgn > 0], drop = FALSE]
            X1miss <- is.na(rowSums(X1))
            X2 <- missing$X2[, !usex[asgn > 0], drop = FALSE]
            X2miss <- is.na(rowSums(X2))
            new.sep <- unique(unlist(list(missing$player1[X1miss],
                                          missing$player2[X2miss])))
            usex <- usex | vars %in% setdiff(sep, new.sep)
        }
        trystat <- try(t(coefs[usex]) %*% chol2inv(chol(vc[usex, usex])) %*%
                       coefs[usex], silent = TRUE)
        if (inherits(trystat, "try-error")) {
            stat[i] <- df[i] <- NA
            tryerror <- TRUE
        }
        else {
            stat[i] <- trystat
            df[i] <- sum(usex)
        }
    }
    table <- data.frame(stat, df)
    dimnames(table) <- list(names(df), c("Statistic", "Df"))
    title <- "Single term deletions\n"
    topnote <- gsub("\\s+", " ", paste("Model: ",
                                     paste(deparse(as.vector(formula(object))),
                                           collapse = ""),
                     if (scale > 0) paste("\nscale: ", format(scale), "\n"),
                     if (tryerror)
                     "\n\nTest statistic unestimable for at least one term"),
                    perl = TRUE)
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
