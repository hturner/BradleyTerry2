#' PQL Estimation of Generalized Linear Mixed Models
#' 
#' Fits GLMMs with simple random effects structure via Breslow and Clayton's
#' PQL algorithm.
#' The GLMM is assumed to be of the form \ifelse{html}{\out{g(<b>&mu;</b>) =
#' <b>X&beta;</b> + <b>Ze</b>}}{\deqn{g(\boldsymbol{\mu}) = \boldsymbol{X\beta}
#' + \boldsymbol{Ze}}{ g(mu) = X * beta + Z * e}} where \eqn{g} is the link
#' function, \ifelse{html}{\out{<b>&mu;</b>}}{\eqn{\boldsymbol{\mu}}{mu}} is the
#' vector of means and \ifelse{html}{\out{<b>X</b>, <b>Z</b>}}{\eqn{\boldsymbol{X},
#' \boldsymbol{Z}}{X,Z}} are design matrices for the fixed effects
#' \ifelse{html}{\out{<b>&beta;</b>}}{\eqn{\boldsymbol{\beta}}{beta}} and random
#' effects \ifelse{html}{\out{<b>e</b>}}{\eqn{\boldsymbol{e}}{e}} respectively.
#' Furthermore the random effects are assumed to be i.i.d.
#' \ifelse{html}{\out{N(0, &sigma;<sup>2</sup>)}}{\eqn{N(0, \sigma^2)}{
#' N(0, sigma^2)}}.
#' 
#' @param fixed a formula for the fixed effects.
#' @param random a design matrix for the random effects, with number of rows
#' equal to the length of variables in \code{formula}.
#' @param family a description of the error distribution and link function to
#' be used in the model. This can be a character string naming a family
#' function, a family function or the result of a call to a family function.
#' (See \code{\link{family}} for details of family functions.)
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{\link{as.data.frame}} to a data frame) containing the variables in
#' the model.  If not found in \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{glmmPQL} called.
#' @param subset an optional logical or numeric vector specifying a subset of
#' observations to be used in the fitting process.
#' @param weights an optional vector of \sQuote{prior weights} to be used in
#' the fitting process.
#' @param offset an optional numeric vector to be added to the linear predictor
#' during fitting. One or more \code{offset} terms can be included in the
#' formula instead or as well, and if more than one is specified their sum is
#' used.  See \code{\link{model.offset}}.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s.  The default is set by the \code{na.action} setting of
#' \code{\link{options}}, and is \code{\link{na.fail}} if that is unset.
#' @param start starting values for the parameters in the linear predictor.
#' @param etastart starting values for the linear predictor.
#' @param mustart starting values for the vector of means.
#' @param control a list of parameters for controlling the fitting process.
#' See the \code{\link{glmmPQL.control}} for details.
#' @param sigma a starting value for the standard deviation of the random
#' effects.
#' @param sigma.fixed logical: whether or not the standard deviation of the
#' random effects should be fixed at its starting value.
#' @param model logical: whether or not the model frame should be returned.
#' @param x logical: whether or not the design matrix for the fixed effects
#' should be returned.
#' @param contrasts an optional list. See the \code{contrasts.arg} argument of
#' \code{\link{model.matrix}}.
#' @param \dots arguments to be passed to \code{\link{glmmPQL.control}}.
#' @return An object of class \code{"BTglmmPQL"} which inherits from
#' \code{"glm"} and \code{"lm"}: \item{coefficients}{ a named vector of
#' coefficients, with a \code{"random"} attribute giving the estimated random
#' effects.} \item{residuals}{ the working residuals from the final iteration
#' of the IWLS loop.} \item{random}{the design matrix for the random effects.}
#' \item{fitted.values}{ the fitted mean values, obtained by transforming the
#' linear predictors by the inverse of the link function.} \item{rank}{the
#' numeric rank of the fitted linear model.} \item{family}{the \code{family}
#' object used.} \item{linear.predictors}{the linear fit on link scale.}
#' \item{deviance}{up to a constant, minus twice the maximized log-likelihood.}
#' \item{aic}{a version of Akaike's \emph{An Information Criterion}, minus
#' twice the maximized log-likelihood plus twice the number of parameters,
#' computed by the \code{aic} component of the family.}
#' \item{null.deviance}{the deviance for the null model, comparable with
#' \code{deviance}.} \item{iter}{the numer of iterations of the PQL algorithm.}
#' \item{weights}{the working weights, that is the weights in the final
#' iteration of the IWLS loop.} \item{prior.weights}{the weights initially
#' supplied, a vector of \code{1}'s if none were.} \item{df.residual}{the
#' residual degrees of freedom.} \item{df.null}{the residual degrees of freedom
#' for the null model.} \item{y}{if requested (the default) the \code{y} vector
#' used. (It is a vector even for a binomial model.)} \item{x}{if requested,
#' the model matrix.} \item{model}{if requested (the default), the model
#' frame.} \item{converged}{logical. Was the PQL algorithm judged to have
#' converged?} \item{call}{the matched call.} \item{formula}{the formula
#' supplied.} \item{terms}{the \code{terms} object used.} \item{data}{the
#' \code{data} argument used.} \item{offset}{the offset vector used.}
#' \item{control}{the value of the \code{control} argument used.}
#' \item{contrasts}{(where relevant) the contrasts used.} \item{xlevels}{(where
#' relevant) a record of the levels of the factors used in fitting.}
#' \item{na.action}{(where relevant) information returned by \code{model.frame}
#' on the special handling of \code{NA}s.} \item{sigma}{the estimated standard
#' deviation of the random effects} \item{sigma.fixed}{logical: whether or not
#' \code{sigma} was fixed} \item{varFix}{the variance-covariance matrix of the
#' fixed effects} \item{varSigma}{the variance of \code{sigma}}
#' @author Heather Turner
#' @seealso
#' \code{\link{predict.BTglmmPQL}},\code{\link{glmmPQL.control}},\code{\link{BTm}}
#' @references Breslow, N. E. and Clayton, D. G. (1993) Approximate inference
#' in Generalized Linear Mixed Models. \emph{Journal of the American
#' Statistical Association} \bold{88}(421), 9--25.
#' 
#' Harville, D. A. (1977) Maximum likelihood approaches to variance component
#' estimation and to related problems. \emph{Journal of the American
#' Statistical Association} \bold{72}(358), 320--338.
#' @keywords models
#' @examples
#' 
#' ###############################################
#' ## Crowder seeds example from Breslow & Clayton
#' ###############################################
#' attach(seeds)
#' 
#' summary(glmmPQL(cbind(r, n - r) ~ seed + extract,
#'         random = diag(length(r)),
#'         family = binomial, data = seeds))
#' 
#' summary(glmmPQL(cbind(r, n - r) ~ seed*extract,
#'                 random = diag(length(r)),
#'                 family = binomial, data = seeds))
#' 
#' @export
glmmPQL <- function(fixed, random = NULL, family = binomial, data = NULL,
                    subset = NULL, weights = NULL, offset = NULL,
                    na.action = NULL,  start = NULL, etastart = NULL,
                    mustart = NULL, control = glmmPQL.control(...),
                    sigma = 0.1, sigma.fixed = FALSE, model = TRUE,
                    x = FALSE, contrasts = NULL, ...) {
    call <-  match.call()
    nm <- names(call)[-1]

    if (is.null(random)) {
        keep <- is.element(nm, c("family", "data", "subset", "weights",
                                 "offset", "na.action"))
        for (i in nm[!keep]) call[[i]] <- NULL
        call$formula <- fixed
        environment(call$formula) <- environment(fixed)
        call[[1]] <- as.name("glm")
        return(eval.parent(call))
    }

    modelTerms <- terms(fixed, data = data)
    modelCall <- as.list(match.call(expand.dots = FALSE))
    argPos <- match(c("data", "subset", "na.action", "weights", "offset"),
                    names(modelCall), 0)
    modelData <- as.call(c(model.frame, list(formula = modelTerms,
                                             drop.unused.levels = TRUE),
                           modelCall[argPos]))
    modelData <- eval(modelData, parent.frame())

    if (!is.null(modelCall$subset))
        Z <- random[eval(modelCall$subset, data, parent.frame()),]
    else Z <- random

    if (!is.null(attr(modelData, "na.action")))
        Z <- Z[-attr(modelData, "na.action"),]

    nObs <- nrow(modelData)
    y <- model.response(modelData, "numeric")
    if (is.null(y))
        y <- rep(0, nObs)
    weights <- as.vector(model.weights(modelData))
    if (!is.null(weights) && any(weights < 0))
        stop("negative weights are not allowed")
    if (is.null(weights))
        weights <- rep.int(1, nObs)
    offset <- as.vector(model.offset(modelData))
    if (is.null(offset))
        offset <- rep.int(0, nObs)

    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("`family' not recognized")
    }
    if (family$family == "binomial") {
        if (is.factor(y) && NCOL(y) == 1)
            y <- y != levels(y)[1]
        else if (NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            weights <- weights * n
        }
    }

    ## Use GLM to estimate fixed effects
    empty <- is.empty.model(modelTerms)
    if (!empty)
        X <- model.matrix(fixed, data = modelData, contrasts)
    else
        X <- matrix(, nObs, 0)
    fit <- glmmPQL.fit(X = X, y = y, Z = Z, weights = weights, start = start,
                       etastart = etastart, mustart = mustart, offset = offset,
                       family = family, control = control, sigma = sigma,
                       sigma.fixed = sigma.fixed, ...)
    if (sum(offset) && attr(modelTerms, "intercept") > 0) {
        fit$null.deviance <- glm.fit(x = X[, "(Intercept)", drop = FALSE],
            y = y, weights = weights, offset = offset, family = family,
            control = control, intercept = TRUE)$deviance
    }
    if (model)
        fit$model <- modelData
    fit$na.action <- attr(modelData, "na.action")
    if (x)
        fit$x <- X
    fit <- c(fit, list(call = call, formula = fixed, random = random,
                       terms = modelTerms,
                       data = data, offset = offset, control = control,
                       method = "glmmPQL.fit", contrasts = attr(X, "contrasts"),
                       xlevels = .getXlevels(modelTerms, modelData)))
    class(fit) <- c("BTglmmPQL", "glm", "lm")
    fit
}

