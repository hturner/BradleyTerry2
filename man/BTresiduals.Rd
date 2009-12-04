\name{BTresiduals}
\alias{BTresiduals}
\title{ player-specific residuals from a Bradley-Terry model }
\description{
  Computes player-specific residuals from a model object of class
  \code{\link{BTm}}, suitable for diagnostic checking of a predictor involving
  player-level covariates.
}
\usage{
BTresiduals(model)
}
\arguments{
  \item{model}{a model object for which \code{inherits(model, "BTm")} is
    \code{TRUE}}
}
\details{
  The residuals returned by \code{BTresiduals} are weighted means of
  working residuals, with weights equal to the binomial denominators in
  the fitted model.  These are suitable for diagnostic model checking,
  for example plotting against candidate predictors.
  }
\value{
  A numeric vector of length equal to the number of players, with a
  "weights" attribute.
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
