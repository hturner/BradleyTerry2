\name{countsToBinomial}
\alias{countsToBinomial}
\title{ Convert Contingency Table of Wins to Binomial Counts }
\description{
  Convert a contingency table of wins to a four-column data frame
  containing the number of wins and losses for each pair of players. 
}
\usage{
countsToBinomial(xtab)
}
\arguments{
  \item{xtab}{ a contingency table of wins cross-classified by
    \dQuote{winner} and \dQuote{loser} }
}
\value{
  A data frame with four columns
  \item{player1 }{ the first player in the contest. }
  \item{player2 }{ the second player in the contest. }
  \item{win1 }{ the number of times \code{player1} won. }
  \item{win2 }{ the number of times \code{player2} won. }
}
\author{ Heather Turner }
\seealso{ \code{\link{BTm}} }
\examples{
########################################################
##  Statistics journal citation data from Stigler (1994)
##  -- see also Agresti (2002, p448)
########################################################
data(citations, package = "BradleyTerry2")
citations

## Convert frequencies to success/failure data
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
citations.sf
}
\keyword{ models }
