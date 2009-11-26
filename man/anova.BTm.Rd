\name{anova.BTm}
\alias{anova.BTm}
\title{ Wald Test of Nested Bradley Terry Models }
\description{
  Computes Wald tests to compare nested models inheriting from class
  \code{"BTm"}.  
}
\usage{
\method{anova}{BTm}(object, ..., dispersion = NULL, test = NULL)
}
\arguments{
  \item{object, \dots}{ a fitted object of class inheriting from
    \code{"BTm"}, or a list of such objects. }
  \item{dispersion}{ a value for the dispersion. Not yet implemented. }
  \item{test}{ optional character string (partially) matching
    \code{"Chisq"}, to specify that p-values should be returned. }
}
\details{
  If a single object is specified, terms are added sequentially and a
  Wald statistic is computed for the extra parameters. Any subterms of
  terms specified by \code{Diff} are added individually, along with any
  necessary \sQuote{separate effects} (see \code{\link{Diff}} for more
  detail).

  For a list of objects, consecutive pairs of models are compared by
  computing a Wald statistic for the extra parameters in the larger of
  the two models.

  The Wald statistic is always based on the variance-covariance matrix
  of the larger of the two models being compared.
}
\value{
  An object of class \code{"anova"} inheriting from class \code{"data.frame"}.
}
\section{Warning}{
  The comparison between two or more models will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is
  used. An error will be returned in this case.
}
\author{ Heather Turner }
\seealso{ \code{\link{BTm}}, \code{\link{Diff}}, \code{\link{add1.BTm}} }
\examples{
data(flatlizards, package = "BradleyTerry2")

attach(flatlizards)
result <- rep(1, nrow(contests))
BTmodel <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
               head.length[..] + (1|..), data = list(contests, predictors),
               trace = TRUE)
anova(BTmodel)
}
\keyword{ models }