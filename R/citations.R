#' Statistics Journal Citation Data from Stigler (1994)
#' 
#' Extracted from a larger table in Stigler (1994).  Inter-journal citation
#' counts for four journals, \dQuote{Biometrika}, \dQuote{Comm Statist.},
#' \dQuote{JASA} and \dQuote{JRSS-B}, as used on p448 of Agresti (2002).
#' 
#' In the context of paired comparisons, the \sQuote{winner} is the cited
#' journal and the \sQuote{loser} is the one doing the citing.
#' 
#' @name citations
#' @docType data
#' @format A 4 by 4 contingency table of citations, cross-classifed by the
#' factors `cited` and `citing` each with levels `Biometrika`,
#' `Comm Statist`, `JASA`, and `JRSS-B`.
#' @seealso [BTm()]
#' @references Firth, D. (2005) Bradley-Terry models in R.  *Journal of
#' Statistical Software* **12**(1), 1--12.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  *Journal of Statistical Software*, **48**(9), 1--21.
#' 
#' Stigler, S. (1994) Citation patterns in the journals of statistics and
#' probability.  *Statistical Science* **9**, 94--108.
#' @source Agresti, A. (2002) *Categorical Data Analysis* (2nd ed).  New
#' York: Wiley.
#' @keywords datasets
#' @examples
#' 
#' ##  Data as a square table, as in Agresti p448
#' citations
#' 
#' ##
#' ## Convert frequencies to success/failure data:
#' ##
#' citations.sf <- countsToBinomial(citations)
#' names(citations.sf)[1:2] <- c("journal1", "journal2")
#' 
#' ## Standard Bradley-Terry model fitted to these data
#' citeModel <-  BTm(cbind(win1, win2), journal1, journal2,
#'                   data = citations.sf)
#' 
"citations"
