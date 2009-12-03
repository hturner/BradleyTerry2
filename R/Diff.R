Diff <- function(player1, player2, formula = NULL, id = "..", data = NULL,
                 #offset = NULL,
                 separate.effect = NULL, refcat = NULL) {
    player.one <- player1[[id]]
    player.two <- player2[[id]]

    if (!is.factor(player.one) || !is.factor(player.two) ||
        !identical(levels(player.one), levels(player.two)))
        stop("'player1$", id, "' and 'player2$", id,
             "' must be factors with the same levels")
    if(is.null(formula)) formula <- reformulate(id)

    players <- levels(player.one)
    nplayers <- nlevels(player.one)
    ncontests <- length(player.one)
    D <- matrix(nrow = ncontests, ncol = nplayers)
    D <- col(D) == as.numeric(player.one)
    D <- D - (col(D) == as.numeric(player.two))
    colnames(D) <- players

    if (formula[[2]] != id) {
        fixed <- lme4:::nobars(formula)
        offset <- missing <- NULL
        X <- matrix(nr = nrow(D), nc = 0)
        if (!is.null(fixed)) {
            mf1 <- model.frame(terms(fixed), data = c(player1, data),
                               na.action = na.pass)
            if (nrow(mf1) != nrow(D))
                stop("Predictor variables are not of the correct length --",
                     "they probably need indexing in 'formula'.")
            offset <- model.offset(mf1)
            predvars <- setdiff(seq(ncol(mf1)),
                                attr(attr(mf1, "terms"), "offset"))
            predvars <- terms(~ . ,data = mf1[, predvars, drop = FALSE])
            X1 <- model.matrix(predvars, mf1)
            X1miss <- is.na(rowSums(X1)) |  player1 %in% separate.effect
            mf2 <- model.frame(terms(fixed), data = c(player2, data),
                               na.action = na.pass)
            if (!is.null(offset)) offset <- offset - model.offset(mf2)
            X2 <- model.matrix(predvars, mf2)
            X2miss <- is.na(rowSums(X2)) |  player2 %in% separate.effect

            X <- missToZero(X1, X1miss) - missToZero(X2, X2miss)
            X <- X[, -1, drop = FALSE]
            attr(X, "assign") <- attr(X1, "assign")[-1]
            if (qr(X)$rank == nplayers &&
                !(id %in% attr(predvars, "term.labels"))) {
                if (is.null(refcat))
                    list(X = D[, -1])
                else
                    list(X = D[, -match(refcat, players)])
            }

            #unique(unlist(list(player.one[X1miss], player.two[X2miss])))
            missing <- unique(c(player.one[X1miss], player.two[X2miss]))
        }
        random <- lme4:::expandSlash(lme4:::findbars(formula[[2]]))
        if (!is.null(random)) {
            if (length(random) > 1 ||
                random[[1]] != parse(text = paste("1|", id, sep = ""))[[1]])
                stop("Currently '(1 | ", id, ")' is the only random effects",
                     "structure allowed.")
            random <- D
        }
        else if (!(id %in% attr(predvars, "term.labels")))
            warning("Ability modelled by predictors but no random effects",
                    call. = FALSE)

        separate.effect <- unique(c(missing, separate.effect))
        if (length(separate.effect)) {
            X <- cbind(D[, separate.effect, drop = FALSE], X)
            attr(X, "assign") <- c(rep(0, length(separate.effect)),
                                   attr(X1, "assign")[-1])
            if (!is.null(random))
                random <- missToZero(D, separate.effect, 2)
        }
        if (length(missing)) {
            Xmiss <- X1miss | X2miss
            Z <- D[, missing]
            attr(Z, "id") <- colnames(Z) # only for simple r.e.
            missing <- list(cases = Xmiss,
                            player1 = player.one[Xmiss],
                            player2 = player.two[Xmiss],
                            X1 = X1[Xmiss,], X2 = X2[Xmiss,], Z = Z)
        }
        return(list(X = X, random = random, offset = offset, missing = missing))
    }

    colnames(D) <- paste(id, colnames(D), sep = "")
    if (is.null(refcat))
        list(X = D[, -1])
    else
        list(X = D[, -match(refcat, players)])
}

