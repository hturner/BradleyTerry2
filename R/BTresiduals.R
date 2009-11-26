BTresiduals <- function(model){
    if (!inherits(model, "BTm"))
        stop("model is not of class BTm")

    if (length(model$random)) {
        mf <- model.frame(model)[[model$random]]
        attrDiff <- attributes(mf)
        player.vars <- attrDiff$model #inc NA
        player.names <- rownames(player.vars)
        X <- mf[,rownames(attrDiff$match.sep)]
        Z <- attrDiff$random
        formula <- update(model$formula, . ~ offset(eta) - 1)
        model <-  glm(formula, family = model$family,
                      data = c(model$data, list(eta = model$linear.predictors)))
    }
    else {
        attrTerms <- attributes(model$terms)
        diffID <- attrTerms$specials$Diff
        diffTerms <- rownames(attrTerms$factors)[diffID]
        if (!length(diffTerms))
            stop("not a Bradley Terry model")
        Z <- model.matrix(model)
        asgn <- attr(Z, "assign")
        diffID <- match(diffTerms, attrTerms$term.labels)
        Z <- Z[, asgn == diffID, drop = FALSE]
    }

    r <- model$residuals  ## the "working" residuals
    w <- model$weights
    total.resid <- crossprod(Z, r * w)
    total.weight <- crossprod(abs(Z), w)
    if (exists("X")) {
        total.resid <- rbind(total.resid,
                             crossprod(X, r * w))[player.names, , drop = FALSE]
        total.weight <- rbind(total.weight,
                              crossprod(abs(X), w))[player.names, , drop = FALSE]
    }
    result <- total.resid / total.weight
    attr(result, "weights") <- total.weight
    result
}
