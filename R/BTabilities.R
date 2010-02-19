BTabilities <-  function (model)
{
    if (!inherits(model, "BTm"))
        stop("model is not of class BTm")

    X0 <- model.matrix(model)
    if (!(model$id %in% model$term.labels)) {
        players <- model$player1[!duplicated(model$player1[, model$id]), ,
                                 drop = FALSE]
        extra <- match(setdiff(levels(model$player1[, model$id]),
                               players[, model$id]),
                       model$player2[, model$id], 0)
        players <- rbind(players, model$player2[extra,, drop = FALSE])
        ## assume player covariates indexed by id
        fixed <- lme4:::nobars(model$formula)
        factors <- attr(terms(fixed), "factors")
        vars <- rownames(factors)
        by.id <- grep(paste("[", model$id, "]", sep = ""), vars,
                      fixed = TRUE)
        drop <- setdiff(seq(length(vars)), by.id)
        ## following will only work for linear terms
        ## (drop any term involving non-player covariate)
        keep <- colSums(factors[drop, , drop = FALSE]) == 0
        formula <- reformulate(names(keep)[keep])
        mf <- model.frame(terms(formula), data = c(players, model$data),
                          na.action = na.pass)
        players <- players[, model$id]
        offset <- model.offset(mf)
        if (is.null(offset)) offset <- 0
        predvars <- setdiff(seq(ncol(mf)),
                            attr(attr(mf, "terms"), "offset"))
        predvars <- terms(~ . ,data = mf[, predvars, drop = FALSE])
        X <- model.matrix(predvars, mf)
        Xmiss <- is.na(rowSums(X)) |  players %in% model$separate.ability
        X[Xmiss, ] <- 0
        X <- X[, -1, drop = FALSE]
        separate.ability <- unique(union(players[Xmiss],
                                        model$separate.ability))
        ns <- length(separate.ability)
        if (ns) {
            S <- matrix(0, nrow = nrow(X), ncol = ns)
            S[cbind(which(players %in% separate.ability), seq(ns))] <- 1
            X <- cbind(S, X)
        }

        kept <- model$assign %in% c(0, which(keep))

        sqrt.vcov <- chol(vcov(model)[kept, kept])
        se <- sqrt(diag(crossprod(sqrt.vcov %*% t(X))))
        abilities <- cbind(X %*% coef(model)[kept] + offset, se)
        rownames(abilities) <- as.character(players)
        attr(abilities, "separate") <- separate.ability
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
    colnames(abilities) <- c("ability", "s.e.")
    rownames(abilities) <- levels(model$player1[, model$id])
    abilities
}
