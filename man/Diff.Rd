\name{Diff}
\alias{Diff}
\title{ Compute Model Matrices for Differenced Player Ability Effects }
\description{
  Computes either a model matrix for the differences between fixed
  ability effects, when a separate ability effect is estimated for each
  player, or a model matrix for the differences between covariate values
  and a model matrix for the differences between random ability effects,
  when player abilities are modelled by covariates.
}
\usage{
Diff(player1, player2, formula = NULL, data = NULL, player.names = NULL,
     offset = NULL, separate.effect = FALSE, refcat = NULL)
}
\arguments{
  \item{player1}{ a factor specifying the first player in each contest.}
  \item{player2}{ a factor specifying the second player in each contest.}
  \item{formula}{ a formula for the player abilities - if
    \code{NULL} a separate ability effect is estimated for each player.}
  \item{offset}{ (optional) offset to \code{formula} - can alternatively
    specify in \code{formula}. }
  \item{data}{ a data frame containing variables in \code{formula} with
    number of rows equal to the number of players. }
  \item{player.names}{ if \code{formula} specified, the player names
    in the same order as the data on the variables in the
    formula. Either a character vector of player names, or a vector of
    length one used to extract the player names from \code{data}. If
    unspecified, the rownames of \code{data} are used. }
  \item{separate.effect}{ a logical vector indicating players for which
    the ability effect is to be modelled as a separate, individual
    effect rather than using the terms specified in \code{formula}
    (ignored if \code{formula} is \code{NULL}). Will be set to
    \code{TRUE} for any players with missing values in one or more of
    the covariates specified in \code{formula}. }
  \item{refcat}{Character.  Which is the "reference" player?  Only used
    when \code{formula} is not \code{NULL}. Default is the name of the
    alphanumerically-first player.}
}
\value{
  If \code{formula} is \code{NULL} or the model specified by
  \code{formula} is saturated, a model matrix for the differences
  between fixed ability effects for each player.

  Otherwise, an object of class \code{"Diff"}, which is a model matrix
  of differences in the covariates specified by \code{formula} between
  \code{player1} and \code{player2}, with attributes used by \code{BTm},
  including: 
  \item{random }{ a model matrix for the differences between random
    ability effects }
  \item{offset }{ if \code{offset} is specified, the difference in
    \code{offset} between \code{player1} and \code{player2} }
  For players with one or more of the covariates missing, the 
  covariate values are set to zero in computing the differences and a
  column is added to the model matrix for estimating a separate fixed
  ability effect.
}
\references{
  Stigler, S. (1994)  Citation patterns in the journals of statistics 
  and probability.  \emph{Statistical Science} \bold{9}, 94--108.
}
\author{ Heather Turner }
\seealso{\code{\link{BTm}}, \code{\link{countsToBinomial}}}
\examples{
########################################################
## Replicating example from ?BTm
## Statistics journal citation data from Stigler (1994)
########################################################

data(citations, package = "BradleyTerry2")
## Convert frequencies to success/failure data
results <- countsToBinomial(xtabs(Freq ~ winner + loser, data = citations))

##  First fit the "standard" Bradley-Terry model
##  BTm and glm give same result
citeModela <- BTm(cbind(won, lost) ~ -1 + Diff(player1, player2),
                  data = results)
citeModelb <- glm(cbind(won, lost) ~ -1 + Diff(player1, player2),
                  family = binomial, data = results)

##  Is the "citeability" of a journal predicted by its country oforigin?
##  glm ignores random effects returned by Diff
journal <- data.frame(origin = factor(c("UK", "USA", "USA", "UK")))
rownames(journal) <- levels(results$player1)
citeModel2a <- BTm(cbind(won, lost) ~ -1 +
                   Diff(player1, player2, ~origin, data = journal),
                   data = results)
citeModel2b <- glm(cbind(won, lost) ~ -1  +
                   Diff(player1, player2, ~origin, data = journal),
                   data = results, family = binomial)
}
\keyword{ models }
