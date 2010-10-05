\name{predict.BTm}
\alias{predict.BTm}
\title{
Predict Method for Bradley-Terry Models
}
\description{
Obtain predictions and optionally standard errors of
those predictions from a fitted Bradley-Terry model.
}
\usage{
\method{predict}{BTm}(object, newdata = NULL, level = 1,
    type = c("link", "response", "terms"), se.fit = FALSE,
    dispersion = NULL, terms = NULL, na.action = na.pass, ...)

}
\arguments{
  \item{object}{a fitted object of class \code{"BTm"}}
  \item{newdata}{(optional) a data frame in which to look for variables
    with which to predict.  If omitted, the fitted linear predictors are
    used.}
  \item{level}{for models with random effects: an integer vector giving
    the level(s) at which predictions are required. Level zero corresponds
    to population-level predictions (fixed effects only), whilst level one
    corresponds to the player-level predictions (full model) which are
    NA for contests involving players not in the original data.}
  \item{type}{the type of prediction required.  The default is on the scale
    of the linear predictors; the alternative \code{"response"} is on
    the scale of the response variable. Thus for a default Bradley-Terry
    model the default predictions are of log-odds (probabilities on
    logit scale) and \code{type = "response"} gives the predicted
    probabilities. The \code{"terms"} option returns a matrix giving the
    fitted values of each term in the model formula on the linear
    predictor scale (fixed effects only).}
  \item{se.fit}{logical switch indicating if standard errors are required.}
  \item{dispersion}{a value for the dispersion, not used for models with
    random effects. If omitted, that returned by \code{summary} applied
    to the object is used, where applicable.}
  \item{terms}{with \code{type ="terms"} by default all terms are returned.  A
    character vector specifies which terms are to be returned.}
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
     If \code{newdata} is omitted the predictions are based on the data used
     for the fit.  In that case how cases with missing values in the
     original fit are treated is determined by the \code{na.action}
     argument of that fit.  If \code{na.action = na.omit} omitted cases
     will not appear in the residuals, whereas if \code{na.action =
       na.exclude} they will appear (in predictions and standard
     errors), with residual value \code{NA}.  See also \code{napredict}.
}
\value{
  If \code{se.fit = FALSE}, a vector or matrix of predictions.  If
  \code{se = TRUE}, a list with components 
  \item{fit }{Predictions}
  \item{se.fit }{Estimated standard errors}
}
\author{
  Heather Turner
}
\seealso{
  \code{\link{predict.glm}},  \code{\link{predict.glmmPQL}}
}
\examples{
## The final model in example(flatlizards)
data(flatlizards, package = "BradleyTerry2")
attach(flatlizards)
Whiting.model3 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                      head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = list(contests, predictors), trace = TRUE)

## `new' data for contests between four of the original lizards
## factor levels must correspond to original levels, but unused levels
## can be dropped - levels must match rows of predictors
newdata  <- list(contests = data.frame(
                 winner = factor(c("lizard048", "lizard060"),
                 levels = c("lizard006", "lizard011", "lizard048", "lizard060")),
                 loser = factor(c("lizard006", "lizard011"),
                 levels = c("lizard006", "lizard011", "lizard048", "lizard060"))
                 ),
                 predictors = predictors[c(3, 6, 27, 33), ])

predict(Whiting.model3, level = 1, newdata = newdata)

## same as
predict(Whiting.model3, level = 1)[1:2]

## introducing a new lizard
newpred <- rbind(predictors[c(3, 6, 27),
                     c("throat.PC1","throat.PC3", "SVL", "head.length")],
                 c(-5, 1.5, 1, 0.1))
rownames(newpred)[4] <- "lizard059"

newdata  <- list(contests = data.frame(
                 winner = factor(c("lizard048", "lizard059"),
                 levels = c("lizard006", "lizard011", "lizard048", "lizard059")),
                 loser = factor(c("lizard006", "lizard011"),
                 levels = c("lizard006", "lizard011", "lizard048", "lizard059"))
                 ),
                 predictors = newpred)

## can only predict at population level for contest with new lizard
predict(Whiting.model3, level = 0:1, se.fit = TRUE, newdata = newdata)

}

\keyword{ models }
