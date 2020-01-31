#' Bradley-Terry Model and Extensions
#' 
#' Fits Bradley-Terry models for pair comparison data, including models with
#' structured scores, order effect and missing covariate data.  Fits by either
#' maximum likelihood or maximum penalized likelihood (with Jeffreys-prior
#' penalty) when abilities are modelled exactly, or by penalized
#' quasi-likelihood when abilities are modelled by covariates.
#' 
#' In each comparison to be modelled there is a 'first player' and a 'second
#' player' and it is assumed that one player wins while the other loses (no
#' allowance is made for tied comparisons).
#' 
#' The [countsToBinomial()] function is provided to convert a
#' contingency table of wins into a data frame of wins and losses for each pair
#' of players.
#' 
#' The `formula` argument specifies the model for player ability and
#' applies to both the first player and the second player in each contest. If
#' `NULL` a separate ability is estimated for each player, equivalent to
#' setting `formula = reformulate(id)`.
#' 
#' Contest-level variables can be specified in the formula in the usual manner,
#' see [formula()]. Player covariates should be included as variables
#' indexed by `id`, see examples. Thus player covariates must be ordered
#' according to the levels of the ID factor.
#' 
#' If `formula` includes player covariates and there are players with
#' missing values over these covariates, then a separate ability will be
#' estimated for those players.
#' 
#' When player abilities are modelled by covariates, then random player effects
#' should be added to the model. These should be specified in the formula using
#' the vertical bar notation of [lme4::lmer()], see examples.
#' 
#' When specified, it is assumed that random player effects arise from a
#' \eqn{N(0, }{N(0, sigma^2)}\eqn{ \sigma^2)}{N(0, sigma^2)} distribution and
#' model parameters, including \eqn{\sigma}{sigma}, are estimated using PQL
#' (Breslow and Clayton, 1993) as implemented in the [glmmPQL()]
#' function.
#' 
#' @param outcome the binomial response: either a numeric vector, a factor in
#' which the first level denotes failure and all others success, or a
#' two-column matrix with the columns giving the numbers of successes and
#' failures.
#' @param player1 either an ID factor specifying the first player in each
#' contest, or a data.frame containing such a factor and possibly other
#' contest-level variables that are specific to the first player. If given in a
#' data.frame, the ID factor must have the name given in the `id`
#' argument. If a factor is specified it will be used to create such a
#' data.frame.
#' @param player2 an object corresponding to that given in `player1` for
#' the second player in each contest, with identical structure -- in particular
#' factors must have identical levels.
#' @param formula a formula with no left-hand-side, specifying the model for
#' player ability. See details for more information.
#' @param id the name of the ID factor.
#' @param separate.ability (if `formula` does not include the ID factor as
#' a separate term) a character vector giving the names of players whose
#' abilities are to be modelled individually rather than using the
#' specification given by `formula`.
#' @param refcat (if `formula` includes the ID factor as a separate term)
#' a character specifying which player to use as a reference, with the first
#' level of the ID factor as the default. Overrides any other contrast
#' specification for the ID factor.
#' @param family a description of the error distribution and link function to
#' be used in the model. Only the binomial family is implemented, with
#' either`"logit"`, `"probit"` , or `"cauchit"` link. (See
#' [stats::family()] for details of family functions.)
#' @param data an optional object providing data required by the model. This
#' may be a single data frame of contest-level data or a list of data frames.
#' Names of data frames are ignored unless they refer to data frames specified
#' by `player1` and `player2`. The rows of data frames that do not
#' contain contest-level data must correspond to the levels of a factor used
#' for indexing, i.e. row 1 corresponds to level 1, etc. Note any rownames are 
#' ignored. Objects are searched for first in the `data` object if
#' provided, then in the environment of `formula`. If `data` is a
#' list, the data frames are searched in the order given.
#' @param weights an optional numeric vector of \sQuote{prior weights}.
#' @param subset an optional logical or numeric vector specifying a subset of
#' observations to be used in the fitting process.
#' @param na.action a function which indicates what should happen when any
#' contest-level variables contain `NA`s. The default is the
#' `na.action` setting of `options`. See details for the handling of
#' missing values in other variables.
#' @param start a vector of starting values for the fixed effects.
#' @param etastart a vector of starting values for the linear predictor.
#' @param mustart a vector of starting values for the vector of means.
#' @param offset an optional offset term in the model. A vector of length equal
#' to the number of contests.
#' @param br logical.  If `TRUE` fitting will be by penalized maximum
#' likelihood as in Firth (1992, 1993), using [brglm::brglm()],
#' rather than maximum likelihood using [glm()], when abilities are
#' modelled exactly or when the abilities are modelled by covariates and the
#' variance of the random effects is estimated as zero.
#' @param model logical: whether or not to return the model frame.
#' @param x logical: whether or not to return the design matrix for the fixed
#' effects.
#' @param contrasts an optional list specifying contrasts for the factors in
#' `formula`. See the `contrasts.arg` of [model.matrix()].
#' @param \dots other arguments for fitting function (currently either
#' [glm()], [brglm::brglm()], or [glmmPQL()])
#' @return An object of class `c("BTm", "x")`, where `"x"` is the
#' class of object returned by the model fitting function (e.g. `glm`).
#' Components are as for objects of class `"x"`, with additionally
#' \item{id}{the `id` argument.} \item{separate.ability}{the
#' `separate.ability` argument.} \item{refcat}{the `refcat`
#' argument.} \item{player1}{a data frame for the first player containing the
#' ID factor and any player-specific contest-level variables.} \item{player2}{a
#' data frame corresponding to that for `player1`.} \item{assign}{a
#' numeric vector indicating which coefficients correspond to which terms in
#' the model.} \item{term.labels}{labels for the model terms.}
#' \item{random}{for models with random effects, the design matrix for the
#' random effects. }
#' @author Heather Turner, David Firth
#' @seealso [countsToBinomial()], [glmmPQL()],
#' [BTabilities()], [residuals.BTm()],
#' [add1.BTm()], [anova.BTm()]
#' @references
#' 
#' Agresti, A. (2002) *Categorical Data Analysis* (2nd ed).  New York:
#' Wiley.
#' 
#' Firth, D. (1992) Bias reduction, the Jeffreys prior and GLIM. In
#' *Advances in GLIM and Statistical Modelling*, Eds. Fahrmeir, L.,
#' Francis, B. J., Gilchrist, R. and Tutz, G., pp91--100. New York: Springer.
#' 
#' Firth, D. (1993) Bias reduction of maximum likelihood estimates.
#' *Biometrika* **80**, 27--38.
#' 
#' Firth, D. (2005) Bradley-Terry models in R.  *Journal of Statistical
#' Software*, **12**(1), 1--12.
#' 
#' Stigler, S. (1994) Citation patterns in the journals of statistics and
#' probability.  *Statistical Science* **9**, 94--108.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  *Journal of Statistical Software*, **48**(9), 1--21.
#' @keywords models
#' @examples
#' 
#' ########################################################
#' ##  Statistics journal citation data from Stigler (1994)
#' ##  -- see also Agresti (2002, p448)
#' ########################################################
#' 
#' ##  Convert frequencies to success/failure data
#' citations.sf <- countsToBinomial(citations)
#' names(citations.sf)[1:2] <- c("journal1", "journal2")
#' 
#' ##  First fit the "standard" Bradley-Terry model
#' citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
#' 
#' ##  Now the same thing with a different "reference" journal
#' citeModel2 <- update(citeModel, refcat = "JASA")
#' BTabilities(citeModel2)
#' 
#' ##################################################################
#' ##  Now an example with an order effect -- see Agresti (2002) p438
#' ##################################################################
#' data(baseball) # start with baseball data as provided by package
#' 
#' ##  Simple Bradley-Terry model, ignoring home advantage:
#' baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
#'                       data = baseball, id = "team")
#' 
#' ##  Now incorporate the "home advantage" effect
#' baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
#' baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
#' baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)
#' 
#' ##  Compare the fit of these two models:
#' anova(baseballModel1, baseballModel2)
#' 
#' ##
#' ## For a more elaborate example with both player-level and contest-level
#' ## predictor variables, see help(chameleons).
#' ##
#' 
#' @importFrom brglm brglm
#' @export
BTm <- function(outcome = 1, player1, player2, formula = NULL,
                id = "..", separate.ability = NULL, refcat = NULL,
                family = "binomial", data = NULL, weights = NULL, subset = NULL,
                na.action = NULL, start = NULL, etastart = NULL, mustart = NULL,
                offset = NULL, br = FALSE, model = TRUE, x = FALSE,
                contrasts = NULL, ...){
    call <- match.call()

    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("`family' not recognized")
    }
    if (family$family != "binomial")
        stop("`family' must be binomial")
    if (!family$link %in% c("logit", "probit", "cauchit"))
        stop("link for binomial family must be one of \"logit\", \"probit\"",
             "or \"cauchit\"")
    fcall <- as.list(match.call(expand.dots = FALSE))
    if (is.null(formula)) {
        formula <- reformulate(id)
        environment(formula) <- parent.frame()
        fcall$formula <- formula
    }
    setup <- match(c("outcome", "player1", "player2", "formula", "id",
                     "separate.ability", "refcat", "data", "weights",
                     "subset", "offset", "contrasts"), names(fcall), 0L)
    setup <- do.call(BTm.setup, fcall[setup], envir = parent.frame())
    if (setup$saturated)
        warning("Player ability saturated - equivalent to fitting ",
                "separate abilities.")
    mf <- data.frame(X = setup$player1) #just to get length
    if (!is.null(setup$X)) {
        mf$X <- setup$X
        formula <- Y ~ X - 1
    }
    else formula <- Y ~ 0
    mf$Y <- setup$Y
    argPos <- match(c("na.action", "start", "etastart",
                      "mustart", "control", "model", "x"), names(fcall), 0)
    dotArgs <- fcall$"..."
    if (is.null(setup$random)) {
        method <- get(ifelse(br, "brglm", "glm"), mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(formula = formula, family = family, data = mf,
                              offset = setup$offset, subset = setup$subset,
                              weights = setup$weights), dotArgs))
        fit <- eval(fit, parent.frame())
    }
    else {
        method <- get("glmmPQL", mode = "function")
        fit <- as.call(c(method, fcall[argPos],
                         list(formula, setup$random, family = family,
                              data = mf, offset = setup$offset,
                              subset = setup$subset, weights = setup$weights), 
                         dotArgs))
        fit <- eval(fit, parent.frame())
        if (br) {
            if (identical(fit$sigma, 0)){
                argPos <- match(c("na.action", "model", "x"), names(fcall), 0)
                method <- get("brglm", mode = "function")
                fit <- as.call(c(method, fcall[argPos],
                                 list(formula, family = family, data = mf,
                                      offset = setup$offset,
                                      subset = setup$subset,
                                      weights = setup$weights,
                                      etastart = fit$linear.predictors)))
                fit <- eval(fit, parent.frame())
                fit$class <- c("glmmPQL", class(fit))
            }
            else
                warning("'br' argument ignored for models with random effects",
                        call. = FALSE)
        }
    }
    if (length(fit$coefficients)) {
        if (ncol(setup$X) > 1)
            names(fit$coefficients) <- substring(names(fit$coefficients), 2)
        else
            names(fit$coefficients) <- colnames(setup$X)
        fit$assign <- attr(setup$X, "assign")
    }
    fit$call <- call
    fit$id <- id
    fit$separate.ability <- separate.ability
    fit$contrasts <- setup$contrasts
    fit$refcat <- setup$refcat
    fit$formula <- setup$formula
    fit$player1 <- setup$player1
    fit$player2 <- setup$player2
    fit$term.labels <- setup$term.labels
    fit$data <- setup$data
    fit$random <- setup$random
    class(fit) <- c("BTm", class(fit))
    fit
}
