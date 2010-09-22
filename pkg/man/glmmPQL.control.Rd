\name{glmmPQL.control}
\alias{glmmPQL.control}
\title{ Control Aspects of the glmmPQL Algorithm }
\description{
  Set control variables for the glmmPQL algorithm.
}
\usage{
glmmPQL.control(maxiter = 50, IWLSiter = 10, tol = 1e-06, trace = FALSE)
}
\arguments{
  \item{maxiter}{ the maximum number of outer iterations. }
  \item{IWLSiter}{ the maximum number of iterated weighted least squares
  iterations used to estimate the fixed effects, given the standard
  deviation of the random effects. }
  \item{tol}{ the tolerance used to determine convergence in the
    IWLS iterations and over all (see details). }
  \item{trace}{ logical: whether or not to print the score for the
    random effects variance at the end of each iteration. } 
}
\details{
  This function provides an interface to control the PQL algorithm used
  by \code{\link{BTm}} for fitting Bradley Terry models with random
  effects.

  The algorithm iterates between a series of iterated weighted least
  squares iterations to update the fixed effects and a single Fisher
  scoring iteration to update the standard deviation of the random
  effects.

  Convergence of both the inner and outer iterations are
  judged by comparing the squared components of the relevant score vector 
  with corresponding elements of the diagonal of the Fisher information
  matrix. If, for all components of the relevant score vector, the ratio
  is less than \code{tolerance^2}, or the corresponding diagonal element
  of the Fisher information matrix is less than 1e-20, iterations cease. 
}
\value{
  A list with the arguments as components.
}
\references{
  Breslow, N. E. and Clayton, D. G. (1993), Approximate Inference in
  Generalized Linear Mixed Models. \emph{JASA} \bold{88(421)}, 9--25.
}
\author{ Heather Turner }
\seealso{\code{\link{glmmPQL}}, \code{\link{BTm}} }
\examples{
## Variation on example(flatlizards)
data(flatlizards)

attach(flatlizards)
result <- rep(1, nrow(contests))

## BTm passes arguments on to glmmPQL.control()
args(BTm)
BTmodel <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
               head.length[..] + SVL[..] + (1|..),
               data = list(contests, predictors), tol = 1e-3, trace = TRUE)
summary(BTmodel)
}
\keyword{ models }
