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
            term.labels <- as.character(sapply(attr(mt, "term.labels"),
                                               as.name))

            vars <- rownames(attr(mt, "factors"))
            indexed <- grep("[[]", vars)
            sep <- list()
            if (length(indexed)) { #set NAs to zero
                indices <- gsub("[^[]*[[]([^]]*)[]]", "\\1", vars[indexed])
                vars <- gsub("([^[]*)[[][^]]*[]]", "\\1", vars[indexed])
                grp <- split(vars, indices)
                for (ind in names(grp)) {
                    vars <- model.frame(terms(reformulate(grp[[ind]])),
                                        data = data, na.action = na.pass)
                    lev <- levels(eval(as.name(i), c(player1, data)))
                    sep[[ind]] <- is.na(rowSums(dat)) | lev %in% separate.effect
                    vars[sep[[ind]], ] <- 0
                    data[grp[[ind]]] <- vars
                }

                ## will need to check for saturation in each set of indexed var
                ## - replacing with sep effects and removing corresponding random effects

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

                fixed <- reformulate(c(indices, attr(mt, "term.labels")))
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

            if (length(indexed)){ #create separate effect factor
                recode <- function(x, keep){
                    lev <- levels(x)
                    ext <- make.unique(c(lev[keep], "nosep"))[sum(keep) + 1]
                    levels(x)[!keep]  <- ext
                    relevel(x, ref = ext)
                }
                for (ind in names(grp)) {
                    mf1[ind] <- recode(mf1[[ind]], sep[[ind]])
                    mf2[ind] <- recode(mf2[[ind]], sep[[ind]])
                }
            }

            X1 <- model.matrix(fixed, mf1, contrasts = contrasts)
            X2 <- model.matrix(fixed, mf2, contrasts = contrasts)
            X <- X1 - X2
            X <- X[, -1, drop = FALSE]
            attr(X, "assign") <- attr(X1, "assign")[-1]
            colnames(X) <- sapply(colnames(X), as.name)
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

        if (length(sep)) {
            attr(X, "assign") <- attr(X, "assign") - 1
            if (!is.null(random))
                random <- D[,!sep[[id]]]
        }
        return(list(X = X, random = random, offset = offset,
                    term.labels = term.labels))
    }

    if (is.null(refcat))
        list(X = D[, -1])
    else
        list(X = D[, -match(refcat, players)])
}

