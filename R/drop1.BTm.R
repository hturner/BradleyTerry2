drop1.BTm <- function(object, scope, scale = 0, test = c("none", "Chisq", "F"),
                      ...) {
    ## Pass on if no random effects
    if (is.null(object$random))
        return(NextMethod())

    ## extend object$formula
    oTerms <- attr(terms(object), "term.labels")
    diffID <- match(object$random, oTerms)
    prefix <- oTerms[diffID]
    m <- model.frame(object)
    oTerms[diffID] <- list(paste("`", prefix,
                                 attr(m[[object$random]], "term.labels"), "`",
                                 sep = ""))
    tl <- unlist(oTerms)

    if (missing(scope) || is.empty.model(scope)) #empty Diff formula?
        scope <- Terms <- ""
    else {
        if (is.character(scope))
            stop("scope must be a formula")
        ##get expanded term labels from scope formula, pass to drop.scope & check against expanded
        ##object formmula - check drop scope also has random effects?

        ## extend scope formula
        Terms <- terms(scope, specials = "Diff")
        attrTerms <- attributes(Terms)
        diffTerms <- rownames(attrTerms$factors)[attrTerms$specials$Diff]

        fc <- object$call
        fc$formula <- Terms
        fob <- list(call = fc, terms = Terms)
        class(fob) <- oldClass(object)
        mf <- model.frame(fob, xlev = object$xlevels)

        Terms <- attr(Terms, "term.labels")
        diffID <- match(diffTerms, Terms)
        Terms[diffID] <- list(paste("`", prefix,
                                    attr(mf[[diffTerms]], "term.labels"), "`",
                                    sep = ""))
        scope <- unlist(Terms)
    }

    scope <- drop.scope(reformulate(setdiff(tl, scope)))
    if (!length(scope))
        stop("no terms in scope for adding to object")
    if (!all(match(scope, tl, 0L) > 0L))
        stop("scope is not a subset of term labels")

    x <- model.matrix(object)
    asgn <- ext.asgn <- attr(x, "assign")
    ext.asgn[ext.asgn == diffID] <-  attr(m[[object$random]], "assign")/10 + diffID #single term
    separate <- is.na(ext.asgn)
    any.sep <- any(separate)
    if (any.sep) {
        match.sep <- list()
        match.sep[[diffID]] <- matrix(0, nr = sum(separate),
                                      nc = length(separate))
        match.sep[[diffID]][,asgn == diffID & !separate] <-
            attr(m[[object$random]], "match.sep")
        match.sep <- do.call("rbind", match.sep)
    }
    diffID <- asgn == diffID
    asgn <- ext.asgn
    asgn[separate] <- 0
    asgn <- match(asgn, unique.default(c(0, asgn))) - 1

    coefs <- coef(object)
    vc <- vcov(object, dispersion = scale) #vcov should deal with dispersion != 1

    sTerms <- sapply(strsplit(scope, ":", fixed = TRUE), function(x) paste(sort(x),
        collapse = ":"))
    stat <- df <- numeric(length(scope))
    names(stat) <- names(df) <- scope
    tryerror <- FALSE
    for (tt in scope) {
        stt <- paste(sort(strsplit(tt, ":")[[1]]), collapse = ":")
        usex <- match(asgn, match(stt, sTerms), 0) > 0
        usex[separate] <- rowSums((!match.sep)[, !usex & !separate & diffID, drop = FALSE]) == 0
        ind <- usex
        trystat <- try(t(coefs[usex]) %*% chol2inv(chol(vc[usex, usex])) %*%
                       coefs[usex], silent = TRUE)
        if (inherits(trystat, "try-error")) {
            stat[tt] <- df[tt] <- NA
            tryerror <- TRUE
        }
        else {
            stat[tt] <- trystat
            df[tt] <- sum(ind)
        }
    }
    table <- data.frame(stat, df)
    names(df) <- gsub("`", "", names(df))
    if (nchar(prefix) > 25) {
        DiffCall <- match.call(Diff, parse(text = prefix))
        short <- paste("Diff(", deparse(DiffCall$player1), ", ",
                       deparse(DiffCall$player2), ", ...)", sep = "")
        names(df) <- gsub(prefix, short, names(df), fixed = TRUE)
    }
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
