\name{BTm}
\alias{BTm}
\title{ Bradley-Terry model and extensions }
\description{
  Fits Bradley-Terry models for pair comparison data, including
  models with structured scores, order effect and missing covariate data.  
  Fits by either maximum likelihood or maximum penalized likelihood
  (with Jeffreys-prior penalty) when abilities are modelled exactly or
  penalized quasi-likelihood when abilities are modelled by covariates.
}
\usage{
BTm(outcome, player1, player2, formula = NULL, id = "..", 
    separate.effect = NULL, refcat = NULL, family = binomial, 
    data = NULL, weights = NULL, subset = NULL, na.action = NULL, 
    start = NULL, etastart = NULL, mustart = NULL, offset = NULL, 
    br = FALSE, model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ...) 
}
\arguments{
  \item{outcome}{ The binomial response: either a numeric vector, a
    factor in which the first level denotes failure and all other
    success, or a two-column matrix with the columns giving the numbers
    of successes and failures. }
  \item{player1}{ Either an ID factor specifying the first player in
    each contest, or a data.frame containing such a factor and possibly
    other contest-level variables that are specific to the first player. If
    given in a data.frame, the ID factor must have the name given in the
    \code{id} argument. If a factor is specified it will be used to
    create such a data.frame. } 
  \item{player2}{ An object corresponding to that given in
    \code{player1} for the second player in each contest, with identical
    structure -- in particular factors must have the same levels. }
  \item{formula}{ An formula with no left-hand-side specifying the
    model for player ability. See details for more information. }
  \item{id}{ The name of the ID factor. } 
  \item{separate.effect}{ A character vector giving the names of
    players whose abilities are to be modelled individually rather than
    using the specification given by \code{formula}. }
  \item{refcat}{(If \code{formula} includes the ID factor as a separate
    term.) A character specifying which player to use as a
    reference. Default is the first level of the ID factor.} 
  \item{family}{ A description of the error distribution and link
    function to be used in the model. Only the binomial family is
    implemented, with either "logit", "probit" , or "cauchit" link. (See
    \code{\link{family}} for details of family functions.)}
  \item{data}{ A list of data frames, possibly including a data frame of
    contest-level variables (with length the number of contests) and
    a data frame of player covariates (with length the number of
    players). Variables are searched for first in
    \code{player1}/\code{player2}, then in the list of data frames in
    the order given, then in the environment of \code{formula}.}
  \item{weights}{
  \item{subset}{  An optional logical or numeric vector specifying a 
    subset of observations to be used in the fitting process. }
  \item{na.action}{ A function which indicates what should happen when
    any contest-level variables contain \code{NA}s. The default is the
    \code{na.action} setting of \code{options}. See details for the handling
    of missing values in other variables. } 
  \item{start}{ A vector of starting values for the fixed effects.}
  \item{etastart}{ A vector of starting values for the linear
    predictor. }
  \item{mustart}{ A vector of starting values for the vector of means.}
  \item{offset}{ An optional offset term in the model. A vector of
    length equal to the number of contests.}
  \item{br}{ Logical.  If \code{TRUE} fitting will be by penalized
    maximum likelihood as in Firth (1992, 1993), using
    \code{\link[brglm]{brglm}}, rather than maximum likelihood using
    \code{\link{glm}}, when abilities are modelled exactly or when the
    abilities are modelled by covariates and the standard error of the
    random effects is estimated as zero. }
  \item{sigma}{ An optional starting value for the standard deviation of
    the random effects. Ignored when abilities are modelled exactly.}
  \item{sigma.fixed}{ Logical indicating whether the standard deviation
    of the random effects should be fixed at the value specified by
    \code{sigma}. }
  \item{control}{ A list of parameters for controlling the fitting
    process. See the documentation for 'BTm.control' for details. }
  \item{\dots}{Other arguments for fitting function (currently either
    \code{\link{glm}}, \code{\link[brglm]{brglm}}, or \code{glmmPQL}) }
}
\details{
  In each comparison to be modelled there is a 'first player' and a
  'second player' and it is assumed that one player wins while the other
  loses (no allowance is made for tied comparisons).

  The \code{\link{countsToBinomial}} function is provided to convert a
  contingency table of wins into a data frame of wins and losses for
  each pair of players.

  The \code{formula} argument specifies the model for player ability and
  applies to both the first player and the second player in each
  contest. If \code{NULL} a separate ability is estimated for each
  player, equivalent to setting \code{formula = reformulate(id)}.

  Contest-level variables can be specified in formula in the usual
  manner, see \code{\link{formula}}. Player covariates should
  be included as variables indexed by \code{id}, see examples. Thus
  player covariates must be ordered according to the levels of the ID
  factor.

  If \code{formula} includes player covariates and there are players
  with missing values over these covariates, then a separate ability
  will be estimated for those players.

  When player abilities are modelled by covariates, then random player
  effects should be added to the model. These should be specified in the
  formula using the vertical bar notation of \code{\link[lme4]{lmer}},
  see examples.

  When specified, it is assumed that random player effects arise from a \eqn{N(0,
    \sigma^2)}{N(0, sigma^2)} distribution and model parameters,
  including \eqn{\sigma}{sigma}, are estimated using PQL (Breslow and
  Clayton, 1993) as implemented in the \code{\link{glmmPQL}} function.
}
\value{
  An object of class \code{c("BTm", "x")}, where \code{"x"} is the class
  of object returned by the model fitting function (e.g. \code{glm}).
  Components are as for objects of class \code{"x"}, with additionally
  \item{id}{The \code{id} argument.}
  \item{separate.effect}{The \code{separate.effect} argument.}
  \item{refcat}{The \code{refcat} argument.}
  \item{player1}{A data.frame for the first player containing the ID
    factor and any player-specific contest-level variables.}
  \item{player2}{A data frame corresponding to that for \code{player1}.}
  \item{assign}{A numeric vector indicating which coefficients
    correspond to which terms in the model.}
  \item{term.labels}{Labels for the model terms.}
  \item{random}{For models with random effects, the design matrix for the
   random effects. }
}
\seealso{
  \code{\link{countsToBinomial}}, \code{\link{glmmPQL}}
  }
\references{

  Agresti, A (2002)  \emph{Categorical Data Analysis} (2nd ed).  New
  York: Wiley.
  
  Breslow, N. E. and Clayton, D. G. (1993) Approximate Inference
  in Generalized Linear Mixed Models. \emph{JASA} \bold{88(421)},
  9--25. 
  
  Firth, D. (1992)  Bias reduction, the Jeffreys prior and GLIM. In 
  \emph{Advances in GLIM and Statistical Modelling}, Eds. L Fahrmeir,
  B J Francis, R Gilchrist and G Tutz, pp91--100.  New York: Springer.
  
  Firth, D. (1993)  Bias reduction of maximum likelihood estimates.
  \emph{Biometrika} \bold{80}, 27--38.
  
  Firth, D. (2005)  Bradley-Terry models in R.  \emph{Journal of Statistical
    Software}, \bold{12}(1).

  Harville, D. A. (1977) Maximum Likelihood Approaches to Variance
  Component Estimation and to Related Problems. \emph{JASA}
  \bold{72(358)}, 320--338. 
  
  Stigler, S. (1994)  Citation patterns in the journals of statistics 
  and probability.  \emph{Statistical Science} \bold{9}, 94--108.
  
}
\author{ Heather Turner, David Firth }
\examples{
########################################################
##  Statistics journal citation data from Stigler (1994)
##  -- see also Agresti (2002, p448)
########################################################
data(citations, package = "BradleyTerry2")

## Convert frequencies to success/failure data
results <- countsToBinomial(xtabs(Freq ~ winner + loser, data = citations))

##  First fit the "standard" Bradley-Terry model
citeModel <- BTm(cbind(won, lost), player1, player2, data = results)

##  Now the same thing with a different "reference" journal
update(citeModel, refcat = "JASA")

##  Is the "citeability" of a journal predicted by its country of origin?
journals <- data.frame(row.names = levels(results$player1),
    origin = factor(c("UK", "USA", "USA", "UK")))
citeModel2 <- BTm(cbind(won, lost), player1, player2,
                  ~ origin[..] + (1|..),
                  data = list(journals, results))
##  Hmm... not so sure about the origin of "Comm Statist" ...
is.na(journals$origin[2]) <- TRUE
citeModel2 <- update(citeModel2, formula = ~ .)

##################################################################
##  Now an example with an order effect -- see Agresti (2002) p438
##################################################################
data(baseball, package = "BradleyTerry2")

##  Simple Bradley-Terry model, ignoring home advantage:
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")

##  Now incorporate the "home advantage" effect
home.team <- with(baseball, data.frame(team = home.team, at.home = 1))
away.team <- with(baseball, data.frame(team = away.team, at.home = 0))
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)

##  Compare the fit of these two models:
anova(baseballModel1, baseballModel2)

}
\keyword{ models }
