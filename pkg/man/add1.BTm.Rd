\name{add1.BTm}
\alias{add1.BTm}
\alias{drop1.BTm}
\title{ Add or Drop Single Terms to/from a Bradley Terry Model }
\description{
  Add or drop single terms within the limit specified by the
  \code{scope} argument. For models with no random effects, compute an
  analysis of deviance table, otherwise compute the Wald statistic of
  the parameters that have been added to or dropped from the model.
}
\usage{
\method{add1}{BTm}(object, scope, scale = 0,
  test = c("none", "Chisq", "F"), x = NULL, ...)

\method{drop1}{BTm}(object, scope, scale = 0,
  test = c("none", "Chisq", "F"), ...)
}

\arguments{
  \item{object}{ a fitted object of class inheriting from \code{"BTm"}. }
  \item{scope}{ a formula specifying the model including all terms to be
    considered for adding or dropping. }
  \item{scale}{ an estimate of the dispersion. Not implemented for
    models with random effects.   }
  \item{test}{ should a p-value be returned? The F test is only
    appropriate for models with no random effects for which the
    dispersion has been estimated. The Chisq test is a likelihood ratio
    test for models with no random effects, otherwise a Wald test. }
  \item{x}{ a model matrix containing columns for all terms in the
    scope.  Useful if \code{add1} is to be called repeatedly.
    \bold{Warning:} no checks are done on its validity. }
  \item{\dots}{ further arguments passed to \code{\link{add1.glm}}. }
}
\details{
  The hierarchy is respected when considering terms to be added or
  dropped: all main effects contained in a second-order interaction must
  remain, and so on.

  In a scope formula \samp{.} means \sQuote{what is already there}.

  For \code{drop1}, a missing \code{scope} is taken to mean that all
  terms in the model may be considered for dropping.

  If \code{scope} includes player covariates and there are players
  with missing values over these covariates, then a separate ability
  will be estimated for these players in \emph{all} fitted
  models. Similarly if there are missing values in any contest-level
  variables in \code{scope}, the corresponding contests will be omitted
  from all models.

  If \code{formula} includes random effects, the same random effects
  structure will apply to all models.
}
\value{
  An object of class \code{"anova"} summarizing the differences in fit
  between the models.
}
\author{ Heather Turner }
\seealso{ \code{\link{BTm}}, \code{\link{anova.BTm}} }
\examples{
attach(flatlizards)
result <- rep(1, nrow(contests))
BTmodel1 <- BTm(result, winner, loser,
                ~ throat.PC1[..] + throat.PC3[..] + (1|..),
                data = list(contests, predictors),
                tol = 1e-4, sigma = 2, trace = TRUE)

drop1(BTmodel1)

add1(BTmodel1, ~ . + head.length[..] + SVL[..], test = "Chisq")

BTmodel2 <- update(BTmodel1, formula = ~ . + head.length[..])

drop1(BTmodel2, test = "Chisq")

}
\keyword{ models }
