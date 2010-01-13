\name{residuals.BTm}
\alias{residuals.BTm}
\title{ Residuals from a Bradley-Terry Model }
\description{
  Computes residuals from a model object of class \code{"BTm"}. In
  additional to the usual options for objects inheriting from class
  \code{"glm"}, a \code{"grouped"} option is implemented to compute
  player-specific residuals suitable for diagnostic checking of a
  predictor involving player-level covariates.
}
\usage{
\method{residuals}{BTm}(object, type = c("deviance", "pearson", "working",
                        "response", "partial", "grouped"),
                        by = object$id, ...)
}
\arguments{ 
  \item{object}{a model object for which \code{inherits(model, "BTm")} is
    \code{TRUE}.}
  \item{type}{the type of residuals which should be returned.  The
    alternatives are: \code{"deviance"} (default), \code{"pearson"},
    \code{"working"}, \code{"response"}, and \code{"partial"}.}
  \item{by}{the grouping factor to use when \code{type = "grouped"}.}
  \item{...}{arguments to pass on other methods.}
}
\details{
  For \code{type} other than \code{"grouped"} see
  \code{\link{residuals.glm}}.

  For \code{type = "grouped"} the residuals
  returned are weighted means of working residuals, with weights equal
  to the binomial denominators in the fitted model.  These are suitable
  for diagnostic model checking, for example plotting against candidate
  predictors. 
}
\value{
  A numeric vector of length equal to the number of players, with a
  \code{"weights"} attribute.
}
\author{ David Firth and Heather Turner }
\references{
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
Software} \bold{12}(1), 1--12.
}
\seealso{ \code{\link{BTm}}, \code{\link{BTabilities}} }
\examples{
data(citations, package = "BradleyTerry2")
##
##  Convert frequencies to success/failure data
##
results <- countsToBinomial(xtabs(Freq ~ winner + loser, data = citations))
##
##  Get journal residuals for the model in which country of origin predicts
##  ability
##
journals <- data.frame(row.names = levels(results$player1),
    origin = factor(c("UK", "USA", "USA", "UK")))
citeModel2 <- BTm(cbind(won, lost), player1, player2,
                  ~ origin[..] + (1|..),
                  data = list(journals, results))
residuals(citeModel2, type = "grouped")
}
\keyword{ models }
