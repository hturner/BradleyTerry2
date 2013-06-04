\name{predict.BTglmmPQL}
\alias{predict.BTglmmPQL}
\title{
Predict Method for BTglmmPQL Objects
}
\description{
Obtain predictions and optionally standard errors of
those predictions from a \code{"BTglmmPQL"} object.
}
\usage{
\method{predict}{BTglmmPQL}(object, newdata = NULL, newrandom = NULL,
    level = 1, type = c("link", "response", "terms"), se.fit = FALSE,
    terms = NULL, na.action = na.pass, ...)
}
\arguments{
  \item{object}{
a fitted object of class \code{"BTglmmPQL"}
}
  \item{newdata}{
(optional) a data frame in which to look for variables
    with which to predict.  If omitted, the fitted linear predictors are
    used.
}
  \item{newrandom}{
    if \code{newdata} is provided, a corresponding design matrix for the
    random effects, will columns corresponding to the random effects
    estimated in the original model.
}
  \item{level}{
    an integer vector giving
    the level(s) at which predictions are required. Level zero corresponds
    to population-level predictions (fixed effects only), whilst level one
    corresponds to the individual-level predictions (full model) which are
    NA for contests involving individuals not in the original data.
}
  \item{type}{
    the type of prediction required.  The default is on the scale
    of the linear predictors; the alternative \code{"response"} is on
    the scale of the response variable. Thus for a default binomial
    model the default predictions are of log-odds (probabilities on
    logit scale) and \code{type = "response"} gives the predicted
    probabilities. The \code{"terms"} option returns a matrix giving the
    fitted values of each term in the model formula on the linear
    predictor scale (fixed effects only).
}
  \item{se.fit}{
    logical switch indicating if standard errors are required.
}
  \item{terms}{
    with \code{type ="terms"} by default all terms are returned.  A
    character vector specifies which terms are to be returned.
}
  \item{na.action}{
    function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.
}
  \item{\dots}{
    further arguments passed to or from other methods.
}
}
\details{
     If \code{newdata} is omitted the predictions are based on the data used
     for the fit.  In that case how cases with missing values in the
     original fit are treated is determined by the \code{na.action}
     argument of that fit.  If \code{na.action = na.omit} omitted cases
     will not appear in the residuals, whereas if \code{na.action =
       na.exclude} they will appear (in predictions and standard
     errors), with residual value \code{NA}.  See also \code{napredict}.

     Standard errors for the predictions are approximated assuming the
     variance of the random effects is known, see Booth and Hobert
     (1998). 
}
\value{
  If \code{se.fit = FALSE}, a vector or matrix of predictions.  If
  \code{se = TRUE}, a list with components 
  \item{fit }{Predictions}
  \item{se.fit }{Estimated standard errors}
}
\references{
  Booth, J. G. and Hobert, J. P. (1998). Standard errors of prediction
  in Generalized Linear Mixed Models. \emph{Journal of the American
    Statistical Association} \bold{93}(441), 262 -- 272.
}
\author{
  Heather Turner
}
\seealso{
  \code{\link{predict.glm}},  \code{\link{predict.BTm}}
}
\examples{
data(seeds)
attach(seeds)

seedsModel <- glmmPQL(cbind(r, n - r) ~ seed + extract,
                      random = diag(length(r)),
                      family = binomial)

pred <- predict(seedsModel, level = 0)
predTerms <- predict(seedsModel, type = "terms")

all.equal(pred, rowSums(predTerms) + attr(predTerms, "constant"))
}
\keyword{ models }
