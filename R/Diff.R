Diff <- function(player1, player2, formula = NULL, id = "..", data = NULL,
                 separate.effect = NULL, refcat = NULL, contrasts = NULL) {
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
    colnames(D) <- sapply(paste(id, players, sep = ""), as.name)

    if (formula[[2]] != id) {
        fixed <- lme4:::nobars(formula)
        offset <- missing <- NULL
        X <- matrix(nr = nrow(D), nc = 0)
        if (!is.null(fixed)) {
            mt <- terms(fixed)
            indexed <- grep("[", rownames(attr(mt, "factors")), fixed = TRUE)
            if (length(indexed)) {
                index.name <- gsub("[^[]*[[]([^]]*)[]]", "\\1",
                                   rownames(attr(mt, "factors")))
                fixed <- reformulate(c(attr(mt, "term.labels"), index.name))
                mt <- terms(fixed)
            }
            mf1 <- model.frame(mt, data = c(player1, data), na.action = na.pass)
            if (nrow(mf1) != nrow(D))
                stop("Predictor variables are not of the correct length --",
                     "they probably need indexing in 'formula'.")
            mf2 <- model.frame(mt, data = c(player2, data), na.action = na.pass)
            offset <- model.offset(mf1)
            if (!is.null(offset)) offset <- offset - model.offset(mf2)
            else offset <- rep(0, nrow(mf2))

            ## set rows corresponding to missing players, judges etc to zero
            ## over appropriate subset of columns
            sep <- list()
            if (length(indexed)){
                sp <- split(indexed, index.name)
                zapfactor <- function(x, keep){lev <- levels(x)
                                               newlev <- make.unique(c(lev, "nosep"))
                                               ext <- newlev[length(newlev)]
                                               levels(x)[!lev %in% keep]  <- ext
                                               x}
                for (index in names(sp)) {
                    hasNA <- {is.na(rowSums(mf1[sp[[index]]])) |
                              mf1[index] %in% separate.effect}
                    mf1[hasNA, sp[[index]]] <- 0
                    sep[[index]] <- mf1[hasNA, index]
                    hasNA <- is.na(rowSums(mf2[sp[[index]]]))
                    mf2[hasNA, sp[[index]]] <- 0
                    sep[[index]] <- unique(union(mf2[hasNA, index], sep[[index]]))
                    browser()
                    mf1[index] <- zapfactor(mf1[[index]], sep[[index]])
                    mf2[index] <- zapfactor(mf2[[index]], sep[[index]])
                }
            }

            X1 <- model.matrix(fixed, mf1, contrasts = contrasts)
            X2 <- model.matrix(fixed, mf2, contrasts = contrasts)
            X <- X1 - X2
            X <- X[, -1, drop = FALSE]
            attr(X, "assign") <- attr(X1, "assign")[-1]
            colnames(X) <- sapply(colnames(X), as.name)
            if (qr(X)$rank == qr(cbind(D, X))$rank &&
                !(id %in% attr(mt, "term.labels"))) {
                message("Ability model saturated, replacing with separate effects.")
                if (is.null(refcat))
                    X <- cbind(D[,-1], X)
                else
                    X <- cbind(D[, -match(refcat, players)], X)

                drop <- rownames(alias(X[,1] ~ . - 1, data.frame(X))$Complete)
                return(list(X = X[, !(make.names(colnames(X)) %in% drop)],
                            offset = offset))
            }
        }
        random <- lme4:::expandSlash(lme4:::findbars(formula[[2]]))
        if (!is.null(random)) {
            if (length(random) > 1 ||
                random[[1]] != parse(text = paste("1|", id, sep = ""))[[1]])
                stop("Currently '(1 | ", id, ")' is the only random effects",
                     "structure allowed.")
            random <- D
        }
        else if (!(id %in% attr(mt, "term.labels")))
            warning("Ability modelled by predictors but no random effects",
                    call. = FALSE)

        term.labels <- as.character(sapply(attr(mt, "term.labels"), as.name))
        if (length(sep)) {
            X <- cbind(D[, sep[[id]], drop = FALSE], X)
            attr(X, "assign") <- c(rep(0, length(sep[[id]])),
                                   attr(X1, "assign")[-1])
            if (!is.null(random))
                random <- missToZero(D, separate.effect, 2)
        }
        if (length(missing)) {
            Xmiss <- X1miss | X2miss
            Z <- D[, missing]
            attr(Z, "id") <- players[missing] # only for simple r.e.
            missing <- list(cases = Xmiss,
                            player1 = player.one[Xmiss],
                            player2 = player.two[Xmiss],
                            X1 = X1[Xmiss, -1], X2 = X2[Xmiss, -1], Z = Z)
        }
        return(list(X = X, random = random, offset = offset, missing = missing,
                    term.labels = term.labels))
    }

    if (is.null(refcat))
        list(X = D[, -1])
    else
        list(X = D[, -match(refcat, players)])
}

