BTabilities <-  function (model, formula = NULL)
{
    if (!inherits(model, "BTm"))
        stop("model is not of class BTm")

    X0 <- model.matrix(model)
    if (qr(X0)$rank != nlevels(model$player1[, model$id]) - 1) {

        players <- model$player1[!duplicated(model$player1[, model$id]),,
                                 drop = FALSE]
        extra <- match(setdiff(players, levels(model$player1[, model$id])),
                       model$player2[, model$id], 0)
        players <- rbind(players, model$player2[extra,, drop = FALSE])

        if (is.null(formula)) { # assume player covariates indexed by id
            fixed <- lme4:::nobars(model$formula)
            mf <- model.frame(terms(fixed), data = c(players, model$data),
                              na.action = na.pass)
            by.id <- grep(paste("[", model$id, "]", sep = ""), colnames(mf))
            drop <- setdiff(seq(ncol(mf)), by.id)
            ## following will only work for linear terms
            keep <- colSums(attr(terms(fixed), "factors")[drop,, drop = FALSE]) == 0
            formula <- reformulate(names(keep)[keep])
        }
        else
            mf <- model.frame(terms(formula), data = c(players, model$data),
                              na.action = na.pass)

        offset <- model.offset(mf)
        if (is.null(offset)) offset <- 0
        predvars <- setdiff(seq(ncol(mf)),
                            attr(attr(mf, "terms"), "offset"))
        predvars <- terms(~ . ,data = mf[, predvars, drop = FALSE])
        X <- model.matrix(predvars, mf)
        Xmiss <- is.na(rowSums(X)) |  players %in% model$separate.effect
        X <- missToZero(X[, -1, drop = FALSE], Xmiss)
        separate.effect <- unique(c(players[Xmiss], model$separate.effect))
        if (length(separate.effect)) {
            sep.fac <- factor(players[players %in% separate.effect],
                              levels = separate.effect)
            X <- cbind(model.matrix(~sep.fac), X)
        }

        kept <- model$assign %in% which(keep)

        sqrt.vcov <- chol(vcov(model)[kept, kept])
        se <- sqrt(diag(crossprod(sqrt.vcov %*% t(X))))
        abilities <- cbind(X %*% coef(model)[kept] + offset, se)
        colnames(abilities) <- c("ability", "s.e.")
        rownames(abilities) <- sapply(as.character(players[, model$id]), as.name)
    }
    else {
        asgn <- model$assign
        if (is.null(asgn))
            abilities <- TRUE
        else
            abilities <- asgn == which(attr(terms(model$formula), "term.labels") == model$id)
        summ <- coef(summary(model))[abilities ,]
        abilities <- cbind(c(0, summ[, 1]), c(0, summ[, 2]))
    }
    abilities
}
