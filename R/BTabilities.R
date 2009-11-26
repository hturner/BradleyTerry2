BTabilities <-  function (model)
{
    if (!inherits(model, "BTm"))
        stop("model is not of class BTm")

    if (!is.null(model$random)) {
        ## assume one diff Term for now
        mf <- model.frame(model)
        attrDiff <- attributes(mf[[model$random]])
        player.vars <- attrDiff$model #inc NA
        player.names <- rownames(player.vars)
        offset <- model.offset(player.vars)
        if (is.null(offset)) offset <- 0
        player.vars <- model.matrix(update(attrDiff$formula, ~ . + 1), player.vars)
        player.vars <- player.vars[, -1, drop = FALSE]
        if (!is.null(attrDiff$match.sep)) {
            which.sep <- match(rownames(attrDiff$match.sep), player.names)
            player.vars[which.sep,] <- 0
        }

        X <- model.matrix(model)
        asgn <- attr(X, "assign")
        termLabels <- attr(model$terms, "term.labels")
        diffID <- match(model$random, termLabels)
        diffLabels <- attr(mf[[model$random]], "term.labels")
        if (length(diffLabels) > 1) {
            termLabels[diffID] <- list(paste(termLabels[diffID],
                                             diffLabels, sep = ""))
            termLabels <- unlist(termLabels)
        }
        ext.asgn <- attr(mf[[model$random]], "assign")/10 + diffID
        separate <- is.na(ext.asgn)
        diffCoefs <- asgn == diffID & !separate
        coefs <- coef(model)[diffCoefs]
        abilities <- player.vars %*% coefs + offset
        se.ability <- rep(0, length(abilities))
        if (length(coefs) > 0) {
            sqrt.vcov <- chol(vcov(model)[diffCoefs, diffCoefs])
            se.ability <- sqrt(diag(crossprod(sqrt.vcov %*% t(player.vars))))
        }
        if (!is.null(attrDiff$match.sep)) {
            abilities[which.sep] <- coef(model)[separate]
            se.ability[which.sep] <- sqrt(diag(vcov(model))[separate])
        }
        separate <- rownames(attrDiff$match.sep)
    }
    else {
        asgn <- attr(model.matrix(model), "assign")
        diffID <- attr(terms(model), "specials")$Diff
        summ <- coef(summary(model))[asgn == diffID - 1,]
        abilities <- c(0, summ[, 1])
        se.ability <- c(0, summ[, 2])
        players <- all.vars(attr(terms(model), "variables")[[diffID + 1]])
        player1 <- as.factor(with(model$data, get(players[1])))
        player2 <- as.factor(with(model$data, get(players[2])))
        player.names <- union(levels(player1), levels(player2))
        separate <- NULL
    }
    abilities <- cbind(abilities, se.ability)
    colnames(abilities) <- c("ability", "s.e.")
    rownames(abilities) <- player.names
    attr(abilities, "separate") <- separate
    abilities
}
