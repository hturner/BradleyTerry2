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
    A formula for the fixed effects.
  }
  \item{random}{
    A design matrix for the random effects, with number of rows equal to
    the length of variables in \code{formula}
  }
  \item{family}{
    A description of the error distribution and link function to
    be used in the model. This can be a character string naming a
    family function, a family function or the result of a call to
    a family function.  (See \code{\link{family}} for details of family
    functions.)
  }
  \item{data}{
    An optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing the
    variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)}, typically
    the environment from which \code{glmmPQL} called.
  }
  \item{subset}{
    An optional logical or numeric vector specifying a 
    subset of observations to be used in the fitting process.
  }
  \item{weights}{
    An optional vector of \sQuote{prior weights} to be used in the
    fitting process. 
  }
  \item{offset}{
    An optional numeric vector to be added to the linear predictor
    during fitting. One or more \code{offset} terms can be included
    in the formula instead or as well, and if more than one is
    specified their sum is used.  See \code{\link{model.offset}}.
}
  \item{na.action}{
    A function which indicates what should happen when the data
    contain \code{NA}s.  The default is set by the \code{na.action} setting
    of \code{\link{options}}, and is \code{\link{na.fail}} if that is unset.
  }
  \item{start}{
    Starting values for the parameters in the linear predictor.
  }
  \item{etastart}{
    Starting values for the linear predictor.
  }
  \item{mustart}{
    Starting values for the vector of means.
  }
  \item{control}{
    A list of parameters for controlling the fitting process.
    See the \code{\link{glmmPQL.control}} for details.
  }
  \item{sigma}{
    A starting value for the standard deviation of the random effects.
  }
  \item{sigma.fixed}{
    Logical: whether or not the standard deviation of the random effects
    should be fixed at its starting value.
  }
  \item{model}{
    Logical: whether or not the model.frame should be returned.
  }
  \item{x}{
    Logical: whether or not the design matrix for the fixed effects
    should be returned.
  }
  \item{contrasts}{
    An optional list. See the \code{contrasts.arg} argument of
    \code{\link{model.matrix}}.
  }
  \item{\dots}{
    Arguments to be passed to \code{\link{glmmPQL.control}}.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  An object of class \code{"glmmPQL"} which inherits from \code{"glm"}
  and \code{"lm"}
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(fixed, random = NULL, family = binomial, data = NULL,
                    subset = NULL, weights = NULL, offset = NULL,
                    na.action = NULL,  start = NULL, etastart = NULL,
                    mustart = NULL, control = glmmPQL.control(...),
                    sigma = 0.1, sigma.fixed = FALSE, model = TRUE,
                    x = FALSE, contrasts = NULL, ...) {
    call <-  match.call()
    nm <- names(call)[-1]

    if (is.null(random)) {
        keep <- is.element(nm, c("family", "data", "subset", "weights",
                                 "offset", "na.action"))
        for (i in nm[!keep]) call[[i]] <- NULL
        call$formula <- fixed
        environment(call$formula) <- environment(fixed)
        call[[1]] <- as.name("glm")
        return(eval.parent(call))
    }

    modelTerms <- terms(fixed, data = data)
    modelCall <- as.list(match.call(expand.dots = FALSE))
    argPos <- match(c("data", "subset", "na.action", "weights", "offset"),
                    names(modelCall), 0)
    modelData <- as.call(c(model.frame, list(formula = modelTerms,
                                             drop.unused.levels = TRUE),
                           modelCall[argPos]))
    modelData <- eval(modelData, parent.frame())

    if (!is.null(modelCall$subset))
        Z <- random[eval(modelCall$subset, data, parent.frame()),]
    else Z <- random

    if (!is.null(attr(modelData, "na.action")))
        Z <- Z[-attr(modelData, "na.action"),]

    nObs <- nrow(modelData)
    y <- model.response(modelData, "numeric")
    if (is.null(y))
        y <- rep(0, nObs)
    weights <- as.vector(model.weights(modelData))
    if (!is.null(weights) && any(weights < 0))
        stop("negative weights are not allowed")
    if (is.null(weights))
        weights <- rep.int(1, nObs)
    offset <- as.vector(model.offset(modelData))
    if (is.null(offset))
        offset <- rep.int(0, nObs)

    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("`family' not recognized")
    }
    if (family$family == "binomial") {
        if (is.factor(y) && NCOL(y) == 1)
            y <- y != levels(y)[1]
        else if (NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            weights <- weights * n
        }
    }

    ## Use GLM to estimate fixed effects
    empty <- is.empty.model(modelTerms)
    if (!empty)
        X <- model.matrix(fixed, data = modelData, contrasts)
    else
        X <- matrix(, nObs, 0)
    fit <- glmmPQL.fit(X = X, y = y, Z = Z, weight = weights, start = start,
                       etastart = etastart, mustart = mustart, offset = offset,
                       family = family, control = control, sigma = sigma,
                       sigma.fixed = sigma.fixed, ...)
    if (sum(offset) && attr(modelTerms, "intercept") > 0) {
        fit$null.deviance <- glm.fit(x = X[, "(Intercept)", drop = FALSE],
            y = y, weights = weights, offset = offset, family = family,
            control = control, intercept = TRUE)$deviance
    }
    if (model)
        fit$model <- modelData
    fit$na.action <- attr(modelData, "na.action")
    if (x)
        fit$x <- X
    fit <- c(fit, list(call = call, formula = fixed, terms = modelTerms,
                       data = data, offset = offset, control = control,
                       method = "glmmPQL.fit", contrasts = attr(X, "contrasts"),
                       xlevels = .getXlevels(modelTerms, modelData)))
    class(fit) <- c("glmmPQL", "glm", "lm")
    fit
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
