#' Specify a Generalised Davidson Term in a gnm Model Formula
#' 
#' GenDavidson is a function of class `"nonlin"` to specify a generalised
#' Davidson term in the formula argument to [gnm::gnm()], providing a
#' model for paired comparison data where ties are a possible outcome.
#' 
#' `GenDavidson` specifies a generalisation of the Davidson model (1970)
#' for paired comparisons where a tie is a possible outcome. It is designed for
#' modelling trinomial counts corresponding to the win/draw/loss outcome for
#' each contest, which are assumed Poisson conditional on the total count for
#' each match. Since this total must be one, the expected counts are
#' equivalently the probabilities for each possible outcome, which are modelled
#' on the log scale: \deqn{\log(p(i \textrm{beats} j)_k) = \theta_{ijk} +
#' \log(\mu\alpha_i}{log(p(i beats j)_k) = theta_{ijk} + log(mu * alpha_i)}
#' \deqn{\log(p(draw)_k) = \theta_{ijk} + \delta + c + }{ log(p(draw)_k) =
#' theta_{ijk} + log(delta) + c + sigma * (pi * log(mu * alpha_i) + (1 - pi) *
#' log(alpha_j)) + (1 - sigma) * log(mu * alpha_i + alpha_j) }\deqn{
#' \sigma(\pi\log(\mu\alpha_i) - (1 - \pi)log(\alpha_j)) + }{ log(p(draw)_k) =
#' theta_{ijk} + log(delta) + c + sigma * (pi * log(mu * alpha_i) + (1 - pi) *
#' log(alpha_j)) + (1 - sigma) * log(mu * alpha_i + alpha_j) }\deqn{ (1 -
#' \sigma)(\log(\mu\alpha_i + \alpha_j))}{ log(p(draw)_k) = theta_{ijk} +
#' log(delta) + c + sigma * (pi * log(mu * alpha_i) + (1 - pi) * log(alpha_j))
#' + (1 - sigma) * log(mu * alpha_i + alpha_j) } \deqn{\log(p(j \textrm{beats}
#' i)_k) = \theta_{ijk} + }{log(p(j beats i)_k) = theta_{ijk} +
#' log(alpha_j)}\deqn{ log(\alpha_j)}{log(p(j beats i)_k) = theta_{ijk} +
#' log(alpha_j)} Here \eqn{\theta_{ijk}}{theta_{ijk}} is a structural parameter
#' to fix the trinomial totals; \eqn{\mu}{mu} is the home advantage parameter;
#' \eqn{\alpha_i}{alpha_i} and \eqn{\alpha_j}{alpha_j} are the abilities of
#' players \eqn{i} and \eqn{j} respectively; \eqn{c}{c} is a function of the
#' parameters such that \eqn{\textrm{expit}(\delta)}{plogis(delta)} is the
#' maximum probability of a tie, \eqn{\sigma}{sigma} scales the dependence of
#' the probability of a tie on the relative abilities and \eqn{\pi}{pi} allows
#' for asymmetry in this dependence.
#' 
#' For parameters that must be positive (\eqn{\alpha_i, \sigma, \mu}{alpha,
#' sigma, mu}), the log is estimated, while for parameters that must be between
#' zero and one (\eqn{\delta, \pi}), the logit is estimated, as illustrated in
#' the example.
#' 
#' @param win a logical vector: `TRUE` if player1 wins, `FALSE`
#' otherwise.
#' @param tie a logical vector: `TRUE` if the outcome is a tie,
#' `FALSE` otherwise.
#' @param loss a logical vector: `TRUE` if player1 loses, `FALSE`
#' otherwise.
#' @param player1 an ID factor specifying the first player in each contest,
#' with the same set of levels as `player2`.
#' @param player2 an ID factor specifying the second player in each contest,
#' with the same set of levels as `player2`.
#' @param home.adv a formula for the paramter corresponding to the home
#' advantage effect. If `NULL`, no home advantage effect is estimated.
#' @param tie.max a formula for the parameter corresponding to the maximum tie
#' probability.
#' @param tie.scale a formula for the parameter corresponding to the scale of
#' dependence of the tie probability on the probability that `player1`
#' wins, given the outcome is not a draw.
#' @param tie.mode a formula for the parameter corresponding to the location of
#' maximum tie probability, in terms of the probability that `player1`
#' wins, given the outcome is not a draw.
#' @param at.home1 a logical vector: `TRUE` if `player1` is at home,
#' `FALSE` otherwise.
#' @param at.home2 a logical vector: `TRUE` if `player2` is at home,
#' `FALSE` otherwise.
#' @return A list with the anticipated components of a "nonlin" function:
#' \item{ predictors }{ the formulae for the different parameters and the ID
#' factors for player 1 and player 2. } \item{ variables }{ the outcome
#' variables and the \dQuote{at home} variables, if specified.  } \item{ common
#' }{ an index to specify that common effects are to be estimated for the
#' players. } \item{ term }{ a function to create a deparsed mathematical
#' expression of the term, given labels for the predictors.} \item{ start }{ a
#' function to generate starting values for the parameters.}
#' @author Heather Turner
#' @seealso [football()], [plotProportions()]
#' @references Davidson, R. R. (1970). On extending the Bradley-Terry model to
#' accommodate ties in paired comparison experiments. *Journal of the
#' American Statistical Association*, **65**, 317--328.
#' @keywords models nonlinear
#' @examples
#' 
#' ### example requires gnm
#' if (require(gnm)) {
#'     ### convert to trinomial counts
#'     football.tri <- expandCategorical(football, "result", idvar = "match")
#'     head(football.tri)
#' 
#'     ### add variable to indicate whether team playing at home
#'     football.tri$at.home <- !logical(nrow(football.tri))
#' 
#'     ### fit shifted & scaled Davidson model
#'     ###  - subset to first and last season for illustration
#'     shifScalDav <- gnm(count ~
#'         GenDavidson(result == 1, result == 0, result == -1,
#'                     home:season, away:season, home.adv = ~1,
#'                     tie.max = ~1, tie.scale = ~1, tie.mode = ~1,
#'                     at.home1 = at.home,
#'                     at.home2 = !at.home) - 1,
#'         eliminate = match, family = poisson, data = football.tri,
#'         subset = season %in% c("2008-9", "2012-13"))
#' 
#'     ### look at coefs
#'     coef <- coef(shifScalDav)
#'     ## home advantage
#'     exp(coef["home.adv"])
#'     ## max p(tie)
#'     plogis(coef["tie.max"])
#'     ## mode p(tie)
#'     plogis(coef["tie.mode"])
#'     ## scale relative to Davidson of dependence of p(tie) on p(win|not a draw)
#'     exp(coef["tie.scale"])
#' 
#'     ### check model fit
#'     alpha <- names(coef[-(1:4)])
#'     plotProportions(result == 1, result == 0, result == -1,
#'                     home:season, away:season,
#'                     abilities = coef[alpha], home.adv = coef["home.adv"],
#'                     tie.max = coef["tie.max"], tie.scale = coef["tie.scale"],
#'                     tie.mode = coef["tie.mode"],
#'                     at.home1 = at.home, at.home2 = !at.home,
#'                     data = football.tri, subset = count == 1)
#' }
#' 
#' ### analyse all five seasons
#' ### - takes a little while to run, particularly likelihood ratio tests
#' \dontrun{
#' ### fit Davidson model
#' Dav <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
#'                                home:season, away:season, home.adv = ~1,
#'                                tie.max = ~1,
#'                                at.home1 = at.home,
#'                                at.home2 = !at.home) - 1,
#'            eliminate = match, family = poisson, data = football.tri)
#' 
#' ### fit scaled Davidson model
#' scalDav <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
#'                                   home:season, away:season, home.adv = ~1,
#'                                   tie.max = ~1, tie.scale = ~1,
#'                                   at.home1 = at.home,
#'                                   at.home2 = !at.home) - 1,
#'                eliminate = match, family = poisson, data = football.tri)
#' 
#' ### fit shifted & scaled Davidson model
#' shifScalDav <- gnm(count ~
#'     GenDavidson(result == 1, result == 0, result == -1,
#'                 home:season, away:season, home.adv = ~1,
#'                 tie.max = ~1, tie.scale = ~1, tie.mode = ~1,
#'                 at.home1 = at.home,
#'                 at.home2 = !at.home) - 1,
#'     eliminate = match, family = poisson, data = football.tri)
#' 
#' ### compare models
#' anova(Dav, scalDav, shifScalDav, test = "Chisq")
#' 
#' ### diagnostic plots
#' main <- c("Davidson", "Scaled Davidson", "Shifted & Scaled Davidson")
#' mod <- list(Dav, scalDav, shifScalDav)
#' names(mod) <- main
#' 
#' ## use football.tri data so that at.home can be found,
#' ## but restrict to actual match results
#' par(mfrow = c(2,2))
#' for (i in 1:3) {
#'     coef <- parameters(mod[[i]])
#'     plotProportions(result == 1, result == 0, result == -1,
#'                     home:season, away:season,
#'                     abilities = coef[alpha],
#'                     home.adv = coef["home.adv"],
#'                     tie.max = coef["tie.max"],
#'                     tie.scale = coef["tie.scale"],
#'                     tie.mode = coef["tie.mode"],
#'                     at.home1 = at.home,
#'                     at.home2 = !at.home,
#'                     main = main[i],
#'                     data = football.tri, subset = count == 1)
#' }
#' }
#' 
#' @importFrom stats coef plogis runif
#' @export
GenDavidson <- function(win, # TRUE/FALSE
                        tie, # TRUE/FALSE
                        loss, # TRUE/FALSE
                        player1, # player1 in each contest
                        player2, # ditto player2
                        home.adv = NULL,
                        tie.max = ~1,
                        tie.mode = NULL,
                        tie.scale = NULL,
                        at.home1 = NULL,
                        at.home2 = NULL){
    call <- as.expression(sys.call()[c(1,5:6)])
    extra <- NULL
    if (is.null(tie.max)) stop("a formula must be specified for tie.max")
    if (!is.null(home.adv) & is.null(at.home1))
        stop("at.home1 and at.home2 must be specified")
    has.home.adv <- !is.null(home.adv)
    has.tie.mode <- !is.null(tie.mode)
    has.tie.scale <- !is.null(tie.scale)
    if (has.home.adv) extra <- c(extra, list(home.adv = home.adv))
    if (has.tie.mode) extra <- c(extra, list(tie.mode = tie.mode))
    if (has.tie.scale) extra <- c(extra, list(tie.scale = tie.scale))
    i <- has.home.adv + has.tie.mode + has.tie.scale
    a <- match("home.adv", names(extra), 1)
    b <- match("tie.mode", names(extra), 1)
    c <- match("tie.scale", names(extra), 1)
    adv <- has.home.adv | has.tie.mode
    list(predictors = {c(extra,
                         list(tie.max = tie.max,
                              substitute(player1), # player1 & 2 are homogeneous
                              substitute(player2)))},
         ## substitutes "result" for "outcome", but also substitutes all of 
         ## code vector
         variables = {c(list(loss = substitute(loss),
                             tie = substitute(tie),
                             win = substitute(win)),
                        list(at.home1 = substitute(at.home1),
                             at.home2 = substitute(at.home2))[adv])},
         common =  c(1[has.home.adv], 2[has.tie.mode], 3[has.tie.scale], 
                     4, 5, 5),
         term = function(predLabels, varLabels){
             if (has.home.adv) {
                 ability1 <- paste("(", predLabels[a], ") * ", varLabels[4],
                                   " + ", predLabels[i + 2], sep = "")
                 ability2 <- paste("(", predLabels[a], ") * ", varLabels[5],
                                   " + ", predLabels[i + 3], sep = "")
             }
             else {
                 ability1 <- predLabels[i + 2]
                 ability2 <- predLabels[i + 3]
             }
             tie.scale <- ifelse(has.tie.scale, predLabels[c], 0)
             scale <- paste("exp(", tie.scale, ")", sep = "")
             if (has.tie.mode) {
                 psi1 <- paste("exp((", predLabels[b], ") * ",  varLabels[4],
                               ")", sep = "")
                 psi2 <- paste("exp((", predLabels[b], ") * ",  varLabels[5],
                               ")", sep = "")
                 weight1 <- paste(psi1, "/(", psi1, " + ", psi2, ")", sep = "")
                 weight2 <- paste(psi2, "/(", psi1, " + ", psi2, ")", sep = "")
             }
             else {
                 weight1 <- weight2 <- "0.5"
             }
             nu <- paste(predLabels[i + 1], " - ", scale, " * (",
                         weight1, " * log(", weight1, ") + ",
                         weight2, " * log(", weight2, "))", sep = "")
             paste(varLabels[1], " * (", ability2, ") + ",
                   varLabels[2], " * (", nu, " + ",
                   scale, " * ", weight1, " * (", ability1, ") + ",
                   scale, " * ", weight2, " * (", ability2, ") + ",
                   "(1 - ", scale, ") * ",
                   "log(exp(", ability1, ") + exp(", ability2, "))) + ",
                   varLabels[3], " * (", ability1, ")", sep = "")
         },
         start = function(theta) {
             init <- runif(length(theta)) - 0.5
             init[c] <- 0.5
         }
         )
}
class(GenDavidson) <- "nonlin"
