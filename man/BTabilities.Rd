\name{BTabilities}
\alias{BTabilities}
\title{ Estimated Abilities in a Bradley-Terry Model }
\description{
   Extracts the \code{abilities} component from a model object of class \code{\link{BTm}}.
}
\usage{
BTabilities(model, formula = NULL)
}
\arguments{
  \item{model}{a model object for which \code{inherits(model, "BTm")} is
    \code{TRUE}}
  \item{formula}{optional: a formula specifying the terms from the
    model that should be used to compute the abilities. By default all
    terms in \code{formula(object)} that are indexed by \code{object$id}
    are included.}
}
\value{
  A two-column numeric matrix, with columns named "ability" and "se";
  one row for each player.

  If the abilities are structured according to a linear predictor, and
  if there are players with missing predictor-variable values, the
  abilities for those players are estimated as separate parameters.  In
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
