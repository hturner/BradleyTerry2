\name{BTabilities}
\alias{BTabilities}
\title{ Estimated Abilities from a Bradley-Terry Model }
\description{
   Computes the (baseline) ability of each player from a model object of
   class \code{\link{BTm}}. 
}
\usage{
BTabilities(model)
}
\arguments{
  \item{model}{a model object for which \code{inherits(model, "BTm")} is
    \code{TRUE}}
}
\value{
  A two-column numeric matrix, with columns named "ability" and "se";
  one row for each player.

  The player abilities are either directly estimated by the model, in
  which case the appropriate parameter estimates are returned, otherwise
  the abilities are computed from the terms of the fitted model that
  involve player covariates only (those indexed by model$id in the model
  formula). Thus parameters in any other terms are treated as zero.   

  If the abilities are structured according to a linear predictor, and
  if there are player covariates with missing values, the
  abilities for the corresponding players are estimated as separate parameters.  In
  this event the resultant matrix has an attribute, named
  \code{"separate"}, which identifies those players whose ability was
  estimated separately.  For an example, see \code{\link{flatlizards}}. 
}
\author{ David Firth and Heather Turner }
\references{
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
Software}, \emph{Journal of Statistical Software} \bold{12}(1), 1-12.  (\url{http://www.jstatsoft.org/v12/a01}).
}
\seealso{ \code{\link{BTm}}, \code{\link{BTresiduals}} }
\examples{
data(citations, package = "BradleyTerry2")

## Convert frequencies to success/failure data
results <- countsToBinomial(xtabs(Freq ~ winner + loser, data = citations))

##  First fit the "standard" Bradley-Terry model
citeModel <- BTm(cbind(won, lost), player1, player2, data = results)
BTabilities(citeModel)

##  Now fit a model in which country of origin predicts "ability"
journals <- data.frame(row.names = levels(results$player1),
    origin = factor(c("UK", "USA", "USA", "UK")))
citeModel2 <- BTm(cbind(won, lost), player1, player2,
                  ~origin[..] + (1|..),
                  data = list(journals, results))
BTabilities(citeModel2)
}
\keyword{ models }
