#' Residuals from a Bradley-Terry Model
#' 
#' Computes residuals from a model object of class \code{"BTm"}. In additional
#' to the usual options for objects inheriting from class \code{"glm"}, a
#' \code{"grouped"} option is implemented to compute player-specific residuals
#' suitable for diagnostic checking of a predictor involving player-level
#' covariates.
#' 
#' For \code{type} other than \code{"grouped"} see \code{\link{residuals.glm}}.
#' 
#' For \code{type = "grouped"} the residuals returned are weighted means of
#' working residuals, with weights equal to the binomial denominators in the
#' fitted model.  These are suitable for diagnostic model checking, for example
#' plotting against candidate predictors.
#' 
#' @param object a model object for which \code{inherits(model, "BTm")} is
#' \code{TRUE}.
#' @param type the type of residuals which should be returned.  The
#' alternatives are: \code{"deviance"} (default), \code{"pearson"},
#' \code{"working"}, \code{"response"}, and \code{"partial"}.
#' @param by the grouping factor to use when \code{type = "grouped"}.
#' @param ... arguments to pass on other methods.
#' @return A numeric vector of length equal to the number of players, with a
#' \code{"weights"} attribute.
#' @author David Firth and Heather Turner
#' @seealso \code{\link{BTm}}, \code{\link{BTabilities}}
#' @references Firth, D. (2005) Bradley-Terry models in R.  \emph{Journal of
#' Statistical Software} \bold{12}(1), 1--12.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  \emph{Journal of Statistical Software}, \bold{48}(9), 1--21.
#' @keywords models
#' @examples
#' 
#' ##
#' ##  See ?springall
#' ##
#' springall.model <- BTm(cbind(win.adj, loss.adj),
#'                        col, row, 
#'                        ~ flav[..] + gel[..] + 
#'                        flav.2[..] + gel.2[..] + flav.gel[..] + (1 | ..),
#'                        data = springall)
#' res <- residuals(springall.model, type = "grouped")
#' with(springall$predictors, plot(flav, res))
#' with(springall$predictors, plot(gel, res))
#' ##  Weighted least-squares regression of these residuals on any variable
#' ##  already included in the model yields slope coefficient zero:
#' lm(res ~ flav, weights = attr(res, "weights"),
#'    data = springall$predictors)
#' lm(res ~ gel, weights = attr(res, "weights"),
#'    data = springall$predictors)
#' 
#' @export
residuals.BTm <- function(object, type = c("deviance", "pearson", "working",
                          "response", "partial", "grouped"), by = object$id,
                          ...) {
    type <- match.arg(type)
    if (type != "grouped") return(NextMethod())

    ## for glm, lm would just be
    ## X <- model.matrix(formula, data = object$data)
    formula <- as.formula(paste("~", by, "- 1"))
    mt <- terms(formula)
    mf1 <- model.frame(mt, data = c(object$player1, object$data))
    X1 <- model.matrix(mt, data = mf1)
    mf2 <- model.frame(mt, data = c(object$player2, object$data))
    X2 <- model.matrix(mt, data = mf2)
    X <- X1 - X2

    r <- object$residuals  ## the "working" residuals
    w <- object$weights
    total.resid <- crossprod(X, r * w)
    total.weight <- crossprod(abs(X), w)
    result <- total.resid / total.weight
    attr(result, "weights") <- total.weight
    result
}
