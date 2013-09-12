BTabilities <-  function (model)
{
    if (!inherits(model, "BTm"))
        stop("model is not of class BTm")

    X0 <- model.matrix(model)
    player1 <- model$player1[, model$id]
    player.names <- levels(player1)
    factors <- attr(terms(model$formula), "factors")
    if (!(model$id %in% rownames(factors))) {
        players <- data.frame(factor(seq(player.names), labels = player.names))
        names(players) <- model$id
        ## assume player covariates indexed by id
        fixed <- nobars(model$formula)
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
        V <- crossprod(sqrt.vcov %*% t(X))
        se <- sqrt(diag(V))
        abilities <- cbind(X %*% coef(model)[kept] + offset, se)
        attr(abilities, "vcov") <- V
        if (length(separate.ability)) {
            attr(abilities, "separate") <- separate.ability
        }
    }
    else {
        asgn <- model$assign
        if (is.null(asgn))
            abilities <- TRUE
        else {
            idterm <- attr(terms(model$formula), "term.labels") == model$id
            if (!any(idterm))
               stop("abilities not uniquely defined for this parameterization")
            coefs.to.include <- asgn[!is.na(coef(model))] == which(attr(terms(model$formula),
                                "term.labels") == model$id)
        }
        summ <- coef(summary(model))[coefs.to.include, , drop = FALSE]
        abilities <- cbind(c(0, summ[, 1]), c(0, summ[, 2]))
        vc <- vcov(model)[coefs.to.include, coefs.to.include,
                          drop = FALSE]
        vc <- rbind(0, cbind(0, vc))
        refcat <- model$refcat
        if (!is.null(refcat)) {
            perm <- order(match(levels(relevel(player1, refcat)),
                                player.names))
            abilities <- abilities[perm, ]
            vc <- vc[perm, perm]
        }
        rownames(vc) <- colnames(vc) <- player.names
        attr(abilities, "vcov") <- vc
    }
    colnames(abilities) <- c("ability", "s.e.")
    rownames(abilities) <- player.names
    attr(abilities, "modelcall") <- model$call
    attr(abilities, "factorname") <- model$id
    class(abilities) <- c("BTabilities", "matrix")
    abilities
}

print.BTabilities <- function(x, ...) {
    attr(x, "vcov") <- attr(x, "modelcall") <- attr(x, "factorname") <- NULL
    class(x) <- "matrix"
    print(x)      ## ie, print without showing the messy attributes
}

vcov.BTabilities <- function(object, ...) {
    attr(object, "vcov")
}

coef.BTabilities <- function(object, ...) {
    object[, "ability"]
}
