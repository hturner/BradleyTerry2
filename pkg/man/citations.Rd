\name{citations}
\alias{citations}
\docType{data}
\title{ Statistics Journal Citation Data from Stigler (1994) }
\description{
  Extracted from a larger table in Stigler (1994).  Inter-journal
  citation counts for four journals, \dQuote{Biometrika},
  \dQuote{Comm Statist.}, \dQuote{JASA} and \dQuote{JRSS-B}, as used
  on p448 of Agresti (2002).
}
\usage{data(citations)}
\format{
  A 4 by 4 contingency table of citations, cross-classifed by the factors
  \code{cited} and \code{citing} each with levels \code{Biometrika},
  \code{Comm Statist}, \code{JASA}, and \code{JRSS-B}.
 }
\details{
  In the context of paired comparisons, the \sQuote{winner} is the cited
  journal and the \sQuote{loser} is the one doing the citing.  
}
\source{
Agresti, A (2002)  \emph{Categorical Data Analysis} (2nd ed).  
New York: Wiley.
}
\references{ 
Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
Software} \bold{12(1)}, 1--12.

Stigler, S. (1994)  Citation patterns in the journals of statistics 
and probability.  \emph{Statistical Science} \bold{9}, 94--108.
}
\seealso{\code{\link{BTm}}}
\examples{
data(citations, package = "BradleyTerry2")
##  Data as a square table, as in Agresti p448
##
## Convert frequencies to success/failure data:
##
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")

## Standard Bradley-Terry model fitted to these data
citeModel <-  BTm(cbind(win1, win2), journal1, journal2,
                  data = citations.sf)
}
\keyword{datasets}
