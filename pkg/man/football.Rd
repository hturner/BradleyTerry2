\name{football}
\alias{football}
\docType{data}
\title{
English Premier League Football Results 2008/9 to 2012/13
}
\description{
The win/lose/draw results for five seasons of the English Premier League
football results, from 2008/9 to 2012/13
}
\usage{football}
\format{
  A data frame with 1881 observations on the following 4 variables.
  \describe{
    \item{\code{season}}{a factor with levels \code{2008-9}, \code{2009-10}, \code{2010-11}, \code{2011-12}, \code{2012-13}}
    \item{\code{home}}{a factor specifying the home team, with 29 levels
      \code{Ars} (Arsenal), ... , \code{Wol} (Wolverhampton)}
    \item{\code{away}}{a factor specifying the away team, with the same
      levels as \code{home}.}
    \item{\code{result}}{a numeric vector giving the result for the home
      team: 1 for a win, 0 for a draw, -1 for a loss.}
  }
}
\details{
  In each season, there are 20 teams, each of which plays one home game
  and one away game against all the other teams in the league. The results
  in 380 games per season.
}
\source{
  \url{http://soccernet.espn.go.com/grid?league=eng.1&season=2011&view=n&cc=5739}
}
\references{
  Davidson, R. R. (1970). On extending the Bradley-Terry model to
  accommodate ties in paired comparison experiments. \emph{Journal of
    the American Statistical Association}, \bold{65}, 317--328.
}
\seealso{\code{\link{GenDavidson}}}
\examples{
### example requires gnm
if (require(gnm)) {
    ### convert to trinomial counts
    football.tri <- expandCategorical(football, "result", idvar = "match")
    head(football.tri)

    ### add variable to indicate whether team playing at home
    football.tri$at.home <- !logical(nrow(football.tri))

    ### fit Davidson model for ties
    Davidson <- gnm(count ~
        GenDavidson(result == 1, result == 0, result == -1,
                    home:season, away:season,
                    home.adv = ~1, tie.max = ~1,
                    at.home1 = at.home, at.home2 = !at.home) - 1,
        eliminate = match, family = poisson, data = football.tri)
}
}
\keyword{datasets}
