\name{add1.BTm}
\alias{add1.BTm}
\alias{drop1.BTm}
\title{ Add or Drop Single Terms to a Bradley Terry Model }
\description{
  Add or drop single terms within the limit specified by the
  \code{scope} argument, including any subterms of terms specified by
  \code{Diff}. Computes the Wald statistic of the parameters that have
  been added or dropped from the model.
}
\usage{
\method{add1}{BTm}(object, scope, scale = 0,
  test = c("none", "Chisq", "F"), x = NULL, ...)

\method{drop1}{BTm}(object, scope, scale = 0, test = c("none", "Chisq", "F"), ...)
}

\arguments{
  \item{object}{ a fitted object of class inheriting from \code{"BTm"}. }
  \item{scope}{ a formula. For \code{add1}, the model including all
    candidate terms and for \code{drop1}, the model excluding all
    candidate terms. }
  \item{scale}{ a value for the dispersion. Not yet implemented. }
  \item{test}{ should a p-value be returned? Only \code{"none"} or
    \code{"Chisq"} implemented. }
  \item{x}{ a model matrix containing columns for all terms in the
    scope.  Useful if \code{add1} is to be called repeatedly.
    \bold{Warning:} no checks are done on its validity. }
  \item{\dots}{ further arguments passed to or from other methods. }
}
\details{
  The hierarchy is respected when considering terms to be added or
  dropped: all main effects contained in a second-order interaction must
  remain, and so on. 

  For \code{drop1}, a missing \code{scope} is taken to be the
  null model with no fixed effects and the same random effects structure
  as the original model.

  Any subterms of terms specified by \code{Diff} are treated as
  individual terms, with corresponding \sQuote{separate effects} added
  or dropped as necessary (see \code{\link{Diff}} for more detail). Thus
  additions or deletions to \code{Diff} terms can be made by specifying
  the \code{Diff} term in the scope with an extended or reduced
  formula, see examples.
}
\value{
  An object of class \code{"anova"} summarizing the differences in fit
  between the models.
}
\author{ Heather Turner }
\seealso{ \code{\link{BTm}}, \code{\link{Diff}}, \code{\link{anova.BTm}} }
\examples{
data(flatlizards, package = "BradleyTerry2")

attach(flatlizards)
result <- rep(1, nrow(contests))
BTmodel1 <- BTm(result, winner, loser,
                ~ throat.PC1[..] + throat.PC3[..] + (1|..),
                data = list(contests, predictors),
                tol = 1e-4, sigma = 2, trace = TRUE)

drop1(BTmodel1)

add1(BTmodel1, ~ throat.PC1[..] + throat.PC3[..] + head.length[..] +
     SVL[..] + (1|..), test = "Chisq")

BTmodel2 <- BTm(result ~ -1 + Diff(winner, loser, ~ throat.PC1 + throat.PC3 +
                                   head.length, data = predictors),
                data = contests, tol = 1e-3, trace = TRUE)

drop1(BTmodel2, ~ -1 + Diff(winner, loser, ~ 0, data = predictors), test = "Chisq")

}
\keyword{ models }
