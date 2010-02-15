\name{baseball}
\alias{baseball}
\docType{data}
\title{ Baseball Data from Agresti (2002) }
\description{
  Baseball results for games in the 1987 season between 7 teams
  in the Eastern Division of the American League.
}
\usage{data(baseball)}
\format{
  A data frame with 42 observations on the following 4 variables.
  \describe{
    \item{\code{home.team}}{a factor with levels \code{Baltimore}, \code{Boston},
      \code{Cleveland}, \code{Detroit},
      \code{Milwaukee},  \code{New York}, \code{Toronto}.}
    \item{\code{away.team}}{a factor with levels \code{Baltimore}, \code{Boston},
      \code{Cleveland}, \code{Detroit},
      \code{Milwaukee},  \code{New York}, \code{Toronto}.}
    \item{\code{home.wins}}{a numeric vector.}
    \item{\code{away.wins}}{a numeric vector.}
  }
}
\source{
Page 438 of Agresti, A (2002) \emph{Categorcal Data Analysis} (2nd Edn.).  
Wiley, New York.
}
\references{
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
Software},  \bold{12}(1).
}
\note{This dataset is in a simpler format than the one described in
  Firth (2005).}
\seealso{\code{\link{BTm}}}
\examples{
##  This reproduces the analysis in Sec 10.6 of Agresti (2002).

data(baseball, package = "BradleyTerry2")

##  Simple Bradley-Terry model, ignoring home advantage:
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")

##  Now incorporate the "home advantage" effect
baseball$home.team <- with(baseball, data.frame(team = home.team, at.home = 1))
baseball$away.team <- with(baseball, data.frame(team = away.team, at.home = 0))
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)

##  Compare the fit of these two models:
anova(baseballModel1, baseballModel2)

}
\keyword{datasets}
