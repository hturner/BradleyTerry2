\name{glmmPQL}
\alias{glmmPQL}
\title{
  PQL Estimation of Generalized Linear Mixed Models
}
\description{
  Fits GLMMs with simple random effects structure via Breslow and
  Clayton's PQL algorithm.
}
\usage{
glmmPQL(fixed, random = NULL, family = binomial, data = NULL,
        subset = NULL, weights = NULL, offset = NULL, na.action = NULL,
        start = NULL, etastart = NULL, mustart = NULL,
        control = glmmPQL.control(...), sigma = 0.1,
        sigma.fixed = FALSE, model = TRUE, x = FALSE, contrasts = NULL,
        ...)  
}
\arguments{
  \item{fixed}{
    a formula for the fixed effects.
  }
  \item{random}{
    a design matrix for the random effects, with number of rows equal to
    the length of variables in \code{formula}.
  }
  \item{family}{
    a description of the error distribution and link function to
    be used in the model. This can be a character string naming a
    family function, a family function or the result of a call to
    a family function.  (See \code{\link{family}} for details of family
    functions.)
  }
  \item{data}{
    an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing the
    variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)}, typically
    the environment from which \code{glmmPQL} called.
  }
  \item{subset}{
    an optional logical or numeric vector specifying a 
    subset of observations to be used in the fitting process.
  }
  \item{weights}{
    an optional vector of \sQuote{prior weights} to be used in the
    fitting process. 
  }
  \item{offset}{
    an optional numeric vector to be added to the linear predictor
    during fitting. One or more \code{offset} terms can be included
    in the formula instead or as well, and if more than one is
    specified their sum is used.  See \code{\link{model.offset}}.
}
  \item{na.action}{
    a function which indicates what should happen when the data
    contain \code{NA}s.  The default is set by the \code{na.action} setting
    of \code{\link{options}}, and is \code{\link{na.fail}} if that is unset.
  }
  \item{start}{
    starting values for the parameters in the linear predictor.
  }
  \item{etastart}{
    starting values for the linear predictor.
  }
  \item{mustart}{
    starting values for the vector of means.
  }
  \item{control}{
    a list of parameters for controlling the fitting process.
    See the \code{\link{glmmPQL.control}} for details.
  }
  \item{sigma}{
    a starting value for the standard deviation of the random effects.
  }
  \item{sigma.fixed}{
    logical: whether or not the standard deviation of the random effects
    should be fixed at its starting value.
  }
  \item{model}{
    logical: whether or not the model frame should be returned.
  }
  \item{x}{
    logical: whether or not the design matrix for the fixed effects
    should be returned.
  }
  \item{contrasts}{
    an optional list. See the \code{contrasts.arg} argument of
    \code{\link{model.matrix}}.
  }
  \item{\dots}{
    arguments to be passed to \code{\link{glmmPQL.control}}.
  }
}
\details{
  The GLMM is assumed to be of the form
  \ifelse{html}{\out{g(<b>&mu;</b>) = <b>X&beta;</b> + <b>Ze</b>}}{
    \deqn{g(\boldsymbol{\mu}) = \boldsymbol{X\beta} + \boldsymbol{Ze}}{
    g(mu) = X * beta + Z * e}}
  where \eqn{g} is the link function, \ifelse{html}{\out{<b>&mu;</b>}}{\eqn{\boldsymbol{\mu}}{mu}} is the
  vector of means and \ifelse{html}{\out{<b>X</b>, <b>Z</b>}}{\eqn{\boldsymbol{X}, \boldsymbol{Z}}{X,Z}} are
  design matrices for the fixed effects \ifelse{html}{\out{<b>&beta;</b>}}{\eqn{\boldsymbol{\beta}}{beta}}
  and random effects \ifelse{html}{\out{<b>e</b>}}{\eqn{\boldsymbol{e}}{e}}
  respectively. Furthermore the random effects are assumed to be
  i.i.d. \ifelse{html}{\out{N(0, &sigma;<sup>2</sup>)}}{\eqn{N(0,
      \sigma^2)}{N(0, sigma^2)}}.
}
\value{
  An object of class \code{"glmmPQL"} which inherits from \code{"glm"}
  and \code{"lm"}. Components are as for \code{"glm"} objects, except
  that there is no \code{effect} component and there is additionally
  \item{sigma}{the estimated standard deviation of the random effects}
  \item{sigma.fixed}{logical: whether or not \code{sigma} was fixed}
  \item{varFix}{the variance-covariance matrix of the fixed effects}
  \item{varSigma}{the variance of \code{sigma}}
}
\references{
  Breslow, N. E. and Clayton, D. G. (1993) Approximate Inference
  in Generalized Linear Mixed Models. \emph{JASA} \bold{88(421)},
  9--25.

  
  Harville, D. A. (1977) Maximum Likelihood Approaches to Variance
  Component Estimation and to Related Problems. \emph{JASA}
  \bold{72(358)}, 320--338.
}
\author{
Heather Turner
}
\seealso{
  \code{\link{glmmPQL.control}},\code{\link{BTm}}
}
\examples{
###############################################
## Crowder seeds example from Breslow & Clayton
###############################################
data(seeds, package = "BradleyTerry2")
attach(seeds)

summary(glmmPQL(cbind(r, n - r) ~ seed + extract,
        random = diag(length(r)),
        family = binomial, data = seeds))

summary(glmmPQL(cbind(r, n - r) ~ seed*extract,
                random = diag(length(r)),
                family = binomial, data = seeds))

}
\keyword{ models }
