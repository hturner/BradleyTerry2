#' Estimated Abilities from a Bradley-Terry Model
#' 
#' Computes the (baseline) ability of each player from a model object of class
#' \code{"BTm"}.
#' 
#' 
#' @aliases BTabilities print.BTabilities coef.BTabilities vcov.BTabilities
#' @param model a model object for which \code{inherits(model, "BTm")} is
#' \code{TRUE}
#' @return A two-column numeric matrix (of class \code{c("BTabilities",
#' "matrix")}, with columns named \code{"ability"} and \code{"se"}; has one row
#' for each player; has attributes named \code{"vcov"}, \code{"modelcall"},
#' \code{"factorname"} and (sometimes --- see below) \code{"separate"}.  The
#' first three attributes are not printed by the method
#' \code{print.BTabilities}.
#' 
#' The player abilities are either directly estimated by the model, in which
#' case the appropriate parameter estimates are returned, otherwise the
#' abilities are computed from the terms of the fitted model that involve
#' player covariates only (those indexed by \code{model$id} in the model
#' formula). Thus parameters in any other terms are assumed to be zero.
#' 
#' If the abilities are structured according to a linear predictor, and if
#' there are player covariates with missing values, the abilities for the
#' corresponding players are estimated as separate parameters.  In this event
#' the resultant matrix has an attribute, named \code{"separate"}, which
#' identifies those players whose ability was estimated separately.  For an
#' example, see \code{\link{flatlizards}}.
#' @author David Firth and Heather Turner
#' @seealso \code{\link{BTm}}, \code{\link{residuals.BTm}}
#' @references Firth, D. (2005) Bradley-Terry models in R.  \emph{Journal of
#' Statistical Software}, \bold{12}(1), 1--12.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  \emph{Journal of Statistical Software}, \bold{48}(9), 1--21.
#' @keywords models
#' @examples
#' 
#' ### citations example
#' 
#' ## Convert frequencies to success/failure data
#' citations.sf <- countsToBinomial(citations)
#' names(citations.sf)[1:2] <- c("journal1", "journal2")
#' 
#' ##  Fit the "standard" Bradley-Terry model
#' citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
#' BTabilities(citeModel)
#' 
#' ### baseball example
#' 
#' baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
#' baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
#' baseballModel2 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
#'                       formula = ~ team + at.home, id = "team",
#'                       data = baseball)
#' ##  Estimated abilities for each team, relative to Baltimore, when
#' ##  playing away from home:  
#' BTabilities(baseballModel2)
#' 
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
        ## remove inestimable coef
        est <- !is.na(model$coef)
        X <- X[, est, drop = FALSE]
        ## keep coef of player covariates
        kept <- model$assign[est] %in% c(0, which(keep))

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
        if (is.null(asgn))
            abilities <- TRUE
        else {
            idterm <- attr(terms(model$formula), "term.labels") == model$id
            if (!any(idterm))
               stop("abilities not uniquely defined for this parameterization")
            coefs.to.include <- asgn == which(idterm)
            vcov.to.include <- asgn[!is.na(coef(model))] == which(idterm)
        }
        coef <- na.exclude(coef(model)[coefs.to.include])
        vc <- vcov(model)[names(coef), names(coef), drop = FALSE]
        ## setup factor reflecting contrasts used ..
        fac <- factor(player.names, labels = paste0(model$id, player.names))
        if (!is.null(model$refcat)) {
            fac <- C(relevel(fac, paste0(model$id, model$refcat)),
                     "contr.treatment")
        } else fac <- C(fac, model$contrasts[[model$id]])
        contr <- contrasts(fac)
        ## calc abilities and s.e., fill in NA as necessary
        if (!is.null(attr(coef, "na.action"))) {
            contr <- contr[, -attr(coef, "na.action"), drop = FALSE]
        }
        est <- contr %*% coef
        ## vc of contrasts for use with qvcalc
        vc <- contr %*% vc %*% t(contr)
        se <- sqrt(diag(vc))
        if (!is.null(attr(coef, "na.action"))){
            id <- match(names(attr(coef, "na.action")), rownames(contr))
            est[id] <- se[id] <- NA
        }
        abilities <- cbind(est, se)
        rownames(abilities) <- levels(fac)
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
