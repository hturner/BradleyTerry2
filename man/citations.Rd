\name{citations}
\alias{citations}
\docType{data}
\title{ Statistics Journal Citation Data from Stigler (1994) }
\description{
  Extracted from a larger table in Stigler (1994).  Inter-journal
  citation counts for four journals, \dQuote{Biometrika}, \dQuote{Comm Statist.}, 
  \dQuote{JASA} and \dQuote{JRSS-B}, as used on p448 of Agresti (2002).
}
\usage{data(citations)}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{winner}}{a factor with levels \code{Biometrika}, \code{Comm Statist}, \code{JASA}, \code{JRSS-B}.}
    \item{\code{loser}}{a factor with levels \code{Biometrika}, \code{Comm Statist}, \code{JASA}, \code{JRSS-B}.}
    \item{\code{Freq}}{a numeric vector.}
  }
}
\details{
  \code{winner} is the cited journal, \code{loser} the one doing the citing.  
}
\source{
Agresti, A (2002)  \emph{Categorical Data Analysis} (2nd ed).  
New York: Wiley.
}
\references{
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
Software}, to appear.

Stigler, S. (1994)  Citation patterns in the journals of statistics 
and probability.  \emph{Statistical Science} \bold{9}, 94--108.
}
\seealso{\code{\link{BTm}}}
\examples{
data(citations, package = "BradleyTerry2")
##  Data as a square table, as in Agresti p448
freq <- xtabs(Freq ~ ., citations)

## Convert frequencies to success/failure data
results <- countsToBinomial(freq)

## Standard Bradley-Terry model fitted to these data
citeModel <-  BTm(cbind(won, lost), player1, player2, data = results)
}
\keyword{datasets}
