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
  A two-column numeric matrix, with columns named \dQuote{ability}
  and \dQuote{se}; one row for each player.

  The player abilities are either directly estimated by the model, in
  which case the appropriate parameter estimates are returned, otherwise
  the abilities are computed from the terms of the fitted model that
  involve player covariates only (those indexed by \code{model$id} in the model
  formula). Thus parameters in any other terms are assumed to be zero.   

  If the abilities are structured according to a linear predictor, and
  if there are player covariates with missing values, the
  abilities for the corresponding players are estimated as separate
  parameters.  In this event the resultant matrix has an attribute, named
  \code{"separate"}, which identifies those players whose ability was
  estimated separately.  For an example, see \code{\link{flatlizards}}. 
}
\author{ David Firth and Heather Turner }
\references{
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
  Software}, \emph{Journal of Statistical Software} \bold{12}(1), 1-12.
  (\url{http://www.jstatsoft.org/v12/a01}).
}
\seealso{ \code{\link{BTm}}, \code{\link{residuals.BTm}} }
\examples{
#########
data(citations, package = "BradleyTerry2")

## Convert frequencies to success/failure data
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")

##  Fit the "standard" Bradley-Terry model
citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
BTabilities(citeModel)

######### 
data(baseball, package = "BradleyTerry2")
baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
baseballModel2 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      formula = ~ team + at.home, id = "team",
                      data = baseball)
##  Estimated abilities for each team, relative to Baltimore, when
##  playing away from home:  
BTabilities(baseballModel2)
}
\keyword{ models }
