\name{anova.BTm}
\alias{anova.BTm}
\title{ Compare Nested Bradley Terry Models }
\description{
  Compare nested models inheriting from class \code{"BTm"}. For models
  with no random effects, compute analysis of deviance table, otherwise
  compute Wald tests of additional terms. 
}
\usage{
\method{anova}{BTm}(object, ..., dispersion = NULL, test = NULL)
}
\arguments{
  \item{object, \dots}{ a fitted object of class inheriting from
    \code{"BTm"}, or a list of such objects. }
  \item{dispersion}{ a value for the dispersion. Not implemented for
    models with random effects. }
  \item{test}{ optional character string (partially) matching one of 
    \code{"Chisq"}, \code{"F"} or \code{"Cp"} to specify that p-values
    should be returned.  The Chisq test is a likelihood ratio
    test for models with no random effects, otherwise a Wald
    test. Options \code{"F"} and \code{"Cp"} are only applicable to
    models with no random effects, see \code{\link{stat.anova}}. }
}
\details{
  For models with no random effects, an analysis of deviance table is
  computed using \code{\link{anova.glm}}. Otherwise, Wald tests are
  computed as detailed here.
  
  If a single object is specified, terms are added sequentially and a
  Wald statistic is computed for the extra parameters. If the full model
  includes player covariates and there are players with missing values
  over these covariates, then the \code{NULL} model will include a
  separate ability for these players. If there are
  missing values in any contest-level variables in the full model, the
  corresponding contests will be omitted throughout. The random effects
  structure of the full model is assumed for all sub-models.

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

  The same problem will occur when separate abilities have been
  estimated for different subsets of players in the models being
  compared. However no warning is given in this case.
}
\author{ Heather Turner }
\seealso{ \code{\link{BTm}}, \code{\link{add1.BTm}} }
\examples{
attach(flatlizards)
result <- rep(1, nrow(contests))
BTmodel <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
               head.length[..] + (1|..), data = list(contests, predictors),
               trace = TRUE)
anova(BTmodel)
}
\keyword{ models }