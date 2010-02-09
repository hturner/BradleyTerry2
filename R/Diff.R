Diff <- function(player1, player2, formula = NULL, id = "..", data = NULL,
                 separate.ability = NULL, refcat = NULL, contrasts = NULL,
                 subset = NULL) {
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
            factors <- attr(mt, "factors")
            term.labels <- as.character(sapply(colnames(factors), as.name))
            vars <- rownames(factors)
            indexed <- grep("[[][^],]+[],]", vars)
            sep <- list()
            if (length(indexed)) { #set NAs to zero
                indices <- gsub("[^[]*[[]([^],]+)[],].*", "\\1", vars[indexed])
                vars <- gsub("([^[]*[[])[^],]+(,.*)", "\\1\\2", vars[indexed])
                vars <- gsub("([^[]*)[[][^],]+.*", "\\1", vars)
                ## assumes no overlap, e.g. no age[..]:judge.gender[judge]
                grp <- split(vars, indices)
                for (ind in names(grp)) {
                    vars <- model.frame(terms(reformulate(grp[[ind]])),
                                        data = data, na.action = na.pass)
                    lev <- levels(eval(as.name(ind), c(player1, data)))
                    as.sep <- rowSums(is.na(vars)) | lev %in% separate.ability
                    if (any(as.sep)) {
                        sep[[ind]] <- as.sep
                        vars[sep[[ind]], ] <- lapply(vars, function(x)
                                                     max(levels(x)[1], 0))
                        data[grp[[ind]]] <- vars
                    }
                }
                if (length(sep)) {
                    fixed <- reformulate(c(names(sep), attr(mt, "term.labels")))
                    mt <- terms(fixed)
                }
            }

            idterm <- id %in% attr(mt, "term.labels")
            mf1 <- model.frame(mt, data = c(player1, data), na.action = na.pass)
            if (nrow(mf1) != nrow(D))
                stop("Predictor variables are not of the correct length --",
                     "they probably need indexing in 'formula'.")
            mf2 <- model.frame(mt, data = c(player2, data), na.action = na.pass)
            if (idterm && !is.null(refcat)){
                mf1[[id]] <- relevel(mf1[[id]], refcat)
                mf2[[id]] <- relevel(mf2[[id]], refcat)
            }
            offset <- model.offset(mf1)
            if (!is.null(offset)) offset <- offset - model.offset(mf2)
            else offset <- rep(0, nrow(mf2))

            if (length(sep)){ #create separate effect factor
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
            colnames(X) <- sapply(colnames(X), as.name)
            ## will need to check for saturation in each set of indexed var
            ## - however as only allowing (1|..) just consider player id for now

            if (qr(na.omit(X))$rank == qr(na.omit(cbind(D, X)))$rank &&
                !idterm) {
                message("Player ability saturated, replacing with separate effects.")
                drop <- indexed[indices == id]
                keep <- !attr(X1, "assign") %in% c(0, drop)
                if (is.null(refcat))
                    X <- cbind(D[,-1], X[, keep])
                else
                    X <- cbind(D[, -match(refcat, players)], X[, keep])
                attr(X, "assign") <- c(rep(0, ncol(D) - 1),
                                       attr(X1, "assign")[keep] + ncol(D) - 1)
                term.labels <- c(id, term.labels[-drop])
                return(list(X = X, offset = offset, term.labels = term.labels))
            }
            X <- X[, -1, drop = FALSE]
            attr(X, "assign") <- attr(X1, "assign")[-1]
        }

        random <- lme4:::expandSlash(lme4:::findbars(formula[[2]]))
        if (!is.null(random)) {
            if (length(random) > 1 ||
                random[[1]] != parse(text = paste("1|", id, sep = ""))[[1]])
                stop("Currently '(1 | ", id, ")' is the only random effects",
                     "structure allowed.")
            random <- D
        }
        else if (!idterm)
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
        list(X = D[, -1], term.labels = id)
    else
        list(X = D[, -match(refcat, players)], term.labels = id)
}

