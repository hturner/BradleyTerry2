#' Estimated Abilities from a Bradley-Terry Model
#' 
#' Computes the (baseline) ability of each player from a model object of class
#' `"BTm"`.
#' 
#' The player abilities are either directly estimated by the model, in which
#' case the appropriate parameter estimates are returned, otherwise the
#' abilities are computed from the terms of the fitted model that involve
#' player covariates only (those indexed by `model$id` in the model
#' formula). Thus parameters in any other terms are assumed to be zero. If one
#' player has been set as the reference, then `predict.BTm()` can be used to 
#' obtain ability estimates with non-player covariates set to other values,
#' see examples for [predict.BTm()].
#' 
#' If the abilities are structured according to a linear predictor, and if
#' there are player covariates with missing values, the abilities for the
#' corresponding players are estimated as separate parameters.  In this event
#' the resultant matrix has an attribute, named `"separate"`, which
#' identifies those players whose ability was estimated separately.  For an
#' example, see [flatlizards()]. 
#' 
#' @aliases BTabilities print.BTabilities coef.BTabilities vcov.BTabilities
#' @param model a model object for which `inherits(model, "BTm")` is
#' `TRUE`
#' @return A two-column numeric matrix of class `c("BTabilities",
#' "matrix")`, with columns named `"ability"` and `"se"`; has one row
#' for each player; has attributes named `"vcov"`, `"modelcall"`,
#' `"factorname"` and (sometimes --- see below) `"separate"`.  The
#' first three attributes are not printed by the method
#' `print.BTabilities`.
#' 
#' @author David Firth and Heather Turner
#' @seealso [BTm()], [residuals.BTm()]
#' @references Firth, D. (2005) Bradley-Terry models in R.  *Journal of
#' Statistical Software*, **12**(1), 1--12.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  *Journal of Statistical Software*, **48**(9), 1--21.
#' @keywords models
#' @examples
#' 
#' ### citations example
#' 
#' ##  Convert frequencies to success/failure data
#' citations.sf <- countsToBinomial(citations)
#' names(citations.sf)[1:2] <- c("journal1", "journal2")
#' 
#' ##  Fit the "standard" Bradley-Terry model
#' citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
#' BTabilities(citeModel)
#' 
#' ### baseball example
#' 
#' data(baseball) # start with baseball data as provided by package
#' 
#' ##  Fit mode with home advantage
#' baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
#' baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
#' baseballModel2 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
#'                       formula = ~ team + at.home, id = "team",
#'                       data = baseball)
#' ##  Estimate abilities for each team, relative to Baltimore, when
#' ##  playing away from home:  
#' BTabilities(baseballModel2)
#' 
#' @importFrom stats C contrasts model.frame model.matrix model.offset na.exclude na.pass terms reformulate relevel vcov
#' @export
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
        rownames(mf) <- player.names
        players <- players[, model$id]
        offset <- model.offset(mf)
        if (is.null(offset)) offset <- 0
        predvars <- setdiff(seq(ncol(mf)),
                            attr(attr(mf, "terms"), "offset"))
        predvars <- reformulate(colnames(mf)[predvars])
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
        ## remove inestimable coef
        est <- !is.na(model$coef)
        kept <- model$assign[est] %in% c(0, which(keep))
        est <- est[kept]
        X <- X[, est, drop = FALSE]

        sqrt.vcov <- chol(vcov(model)[kept, kept])
        V <- crossprod(sqrt.vcov %*% t(X))
        se <- sqrt(diag(V))
        abilities <- cbind(X %*% coef(model)[est][kept] + offset, se)
        attr(abilities, "vcov") <- V
        if (length(separate.ability)) {
            attr(abilities, "separate") <- separate.ability
        }
    }
    else {
        ## get ability coef and corresponding vcov
        asgn <- model$assign
        if (is.null(asgn)){
            abilities <- TRUE
        } else {
            idterm <- attr(terms(model$formula), "term.labels") == model$id
            if (!any(idterm))
               stop("abilities not uniquely defined for this parameterization")
            coefs.to.include <- asgn == which(idterm)
            vcov.to.include <- asgn[!is.na(coef(model))] == which(idterm)
        }
        coef <- na.exclude(coef(model)[coefs.to.include])
        vc <- vcov(model)[names(coef), names(coef), drop = FALSE]
        ## setup factor reflecting contrasts used ..
        fac <- factor(player.names, levels = player.names)
        if (!is.null(model$refcat)) {
            fac <- C(relevel(fac, model$refcat),
                     "contr.treatment")
        } else fac <- C(fac, model$contrasts[[model$id]])
        contr <- contrasts(fac)[player.names,]
        ## calc abilities and s.e., fill in NA as necessary
        if (!is.null(attr(coef, "na.action"))) {
            contr <- contr[, -attr(coef, "na.action"), drop = FALSE]
        }
        est <- contr %*% coef
        ## vc of contrasts for use with qvcalc
        vc <- contr %*% vc %*% t(contr)
        se <- sqrt(diag(vc))
        if (!is.null(attr(coef, "na.action"))){
            id <- match(names(attr(coef, "na.action")), 
                        paste0(model$id, rownames(contr)))
            est[id] <- se[id] <- NA
        }
        abilities <- cbind(est, se)
        rownames(abilities) <- player.names
        attr(abilities, "vcov") <- vc
    }
    colnames(abilities) <- c("ability", "s.e.")
    attr(abilities, "modelcall") <- model$call
    attr(abilities, "factorname") <- model$id
    class(abilities) <- c("BTabilities", "matrix")
    abilities
}

#' @export
print.BTabilities <- function(x, ...) {
    attr(x, "vcov") <- attr(x, "modelcall") <- attr(x, "factorname") <- NULL
    class(x) <- "matrix"
    print(x)      ## ie, print without showing the messy attributes
}

#' @export
vcov.BTabilities <- function(object, ...) {
    attr(object, "vcov")
}

#' @export
coef.BTabilities <- function(object, ...) {
    object[, "ability"]
}
