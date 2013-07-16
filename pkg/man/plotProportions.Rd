\name{plotProportions}
\alias{plotProportions}
\title{
Plot Proportions of Tied Matches and Non-tied Matches Won
}
\description{
Plot proportions of tied matches and non-tied matches won by the first
player, within matches binned by the relative player ability, as
expressed by the probability that the first player wins, given the match
is not a tie. Add fitted lines for each set of matches, as given by the
generalized Davidson model.
}
\usage{
plotProportions(win, tie = NULL, loss, player1, player2, abilities = NULL,
    home.adv = NULL, tie.max = NULL, tie.scale = NULL, tie.mode = NULL,
    at.home1 = NULL, at.home2 = NULL, data = NULL, subset = NULL,
    bin.size = 20, xlab = "P(player1 wins | not a tie)",
    ylab = "Proportion", legend = NULL, col = 1:2, ...)
}
\arguments{
  \item{win}{ a logical vector: \code{TRUE} if player1 wins,
    \code{FALSE} otherwise.
}
  \item{tie}{ a logical vector: \code{TRUE} if the outcome is a tie,
    \code{FALSE} otherwise (\code{NULL} if there are no ties).
}
  \item{loss}{ a logical vector: \code{TRUE} if player1 loses,
    \code{FALSE} otherwise.
}
  \item{player1}{ an ID factor specifying the first player in
    each contest, with the same set of levels as \code{player2}.
}
  \item{player2}{ an ID factor specifying the second player in
    each contest, with the same set of levels as \code{player2}.
}
  \item{abilities}{ the fitted abilities from a
    generalized Davidson model (or a Bradley-Terry model).
}
  \item{home.adv}{ if applicable, the fitted home advantage parameter from a
    generalized Davidson model (or a Bradley-Terry model).
}
  \item{tie.max}{ the fitted parameter from a generalized Davidson
    model corresponding to the  maximum tie probability.
}
  \item{tie.scale}{ if applicable, the fitted parameter from a
    generalized Davidson model corresponding to the
    scale of dependence of the tie probability on the probability
    that \code{player1} wins, given the outcome is not a draw.
}
  \item{tie.mode}{ if applicable, the fitted parameter from a
    generalized Davidson model corresponding to the
    location of maximum tie probability, in terms of the probability
    that \code{player1} wins, given the outcome is not a draw.
}
  \item{at.home1}{ a logical vector: \code{TRUE} if \code{player1} is at
    home, \code{FALSE} otherwise.
}
  \item{at.home2}{ a logical vector: \code{TRUE} if \code{player2} is at
    home, \code{FALSE} otherwise.
}
  \item{data}{  an optional data frame providing variables required by the
    model, with one observation per match.
}
  \item{subset}{ an optional logical or numeric vector specifying a 
    subset of observations to include in the plot.
}
  \item{bin.size}{ the approximate number of matches in each bin.
}
  \item{xlab}{ the label to use for the x-axis.
}
  \item{ylab}{ the label to use for the y-axis.
  }
  \item{legend}{ text to use for the legend.
  }
  \item{col}{ a vector specifying colours to use for the proportion of
    non-tied matches won and the proportion of tied matches.
    }
  \item{\dots}{ further arguments passed to plot.
}
}
\details{
  If \code{home.adv} is specified, the results are re-ordered if
  necessary so that the home player comes first; any matches played on
  neutral ground are omitted.

  First the probability that the first player wins given that the match
  is not a tie is computed:
  \deqn{expit(home.adv + abilities[player1] - abilities[player2])}
  where \code{home.adv} and \code{abilities} are parameters from a
  generalized Davidson model that have been estimated on the log scale.

  The matches are then binned according to this probability, grouping
  together matches with similar relative ability between the first player and the
  second player. Within each bin, the proportion of tied matches is
  computed and these proportions are plotted against the mid-point of
  the bin. Then the bins are re-computed omitting the tied games and the
  proportion of non-tied matches won by the first player is found and
  plotted against the new mid-point.

  Finally curves are added for the probability of a tie and the
  conditional probability of win given the match is not a tie, under a
  generalized Davidson model with parameters as specified by
  \code{tie.max}, \code{tie.scale} and \code{tie.mode}.

  The function can also be used to plot the proportions of wins along
  with the fitted probability of a win under the Bradley-Terry model. 
}
\note{
  This function is designed for single match outcomes, therefore data
  aggregated over player pairs will need to be expanded.
}
\value{
  A list of data frames:
  \item{win}{ a data frame comprising \code{prop.win}, the proportion of
  non-tied matches won by the first player in each bin and
  \code{bin.win}, the mid-point of each bin. }
  \item{tie}{ (when ties are present) a data frame comprising
  \code{prop.tie}, the proportion of tied matches in each bin and
  \code{bin.tie}, the mid-point of each bin. }
}
\author{
  Heather Turner
}
\seealso{
   \code{\link{GenDavidson}}, \code{\link{BTm}}
}
\examples{
#### A Bradley-Terry example
data(icehockey, package = "BradleyTerry2")

## Fit the standard Bradley-Terry model, ignoring home advantage
standardBT <- BTm(outcome = result,
                  player1 = visitor, player2 = opponent,
                  id = "team", data = icehockey)

## comparing teams on a "level playing field"
levelBT <- BTm(result,
               data.frame(team = visitor, home.ice = 0),
               data.frame(team = opponent, home.ice = home.ice),
               ~ team + home.ice,
               id = "team", data = icehockey)

## compare fit to observed proportion won
## exclude tied matches as not explicitly modelled here
par(mfrow = c(1, 2))
plotProportions(win = result == 1, loss = result == 0,
                player1 = visitor, player2 = opponent,
                abilities = BTabilities(standardBT)[,1],
                data = icehockey, subset = result != 0.5,
                main = "Without home advantage")

plotProportions(win = result == 1, loss = result == 0,
                player1 = visitor, player2 = opponent,
                home.adv = coef(levelBT)["home.ice"],
                at.home1 = 0, at.home2 = home.ice,
                abilities = BTabilities(levelBT)[,1],
                data = icehockey, subset = result != 0.5,
                main = "With home advantage")

#### A generalized Davidson example
data(football)

## convert to trinomial counts
library(gnm)
football.tri <- expandCategorical(football, "result", idvar = "match")

## add variable to indicate whether team playing at home
football.tri$at.home <- !logical(nrow(football.tri))


## fit Davidson model for ties
Dm0 <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
                              home:season, away:season, home.adv = ~1,
                              tie.max = ~1,
                              at.home1 = at.home,
                              at.home2 = !at.home) - 1,
          eliminate = match, family = poisson, data = football.tri)

## fit scaled Davidson model
Dm1 <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
                              home:season, away:season, home.adv = ~1,
                              tie.max = ~1, tie.scale = ~1,
                              at.home1 = at.home,
                              at.home2 = !at.home) - 1,
           eliminate = match, family = poisson, data = football.tri)

## fit shifted & scaled Davidson model
Dm2 <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
                               home:season, away:season, home.adv = ~1,
                               tie.max = ~1, tie.scale = ~1, tie.mode = ~1,
                               at.home1 = at.home,
                               at.home2 = !at.home) - 1,
           eliminate = match, family = poisson, data = football.tri)

## diagnostic plots
main <- c("Davidson", "Scaled Davidson", "Shifted & Scaled Davidson")
mod <- list(Dm0, Dm1, Dm2)
names(mod) <- main
alpha <- names(coef(Dm0)[-(1:2)])

## use football.tri data so that at.home can be found,
## but restrict to actual match results
par(mfrow = c(2,2))
for (i in 1:3) {
    coef <- parameters(mod[[i]])
    plotProportions(result == 1, result == 0, result == -1,
                    home:season, away:season,
                    abilities = coef[alpha],
                    home.adv = coef["home.adv"],
                    tie.max = coef["tie.max"],
                    tie.scale = coef["tie.scale"],
                    tie.mode = coef["tie.mode"],
                    at.home1 = at.home,
                    at.home2 = !at.home,
                    main = main[i],
                    data = football.tri, subset = count == 1)
}

}
\keyword{ models }
\keyword{ nonlinear }
