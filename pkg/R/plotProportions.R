## P(win|not tie) in terms of expit(lambda_i - lambda_j)
GenDavidsonTie <- function(p){
    scale <- match("tie.scale", substring(names(coef), 1, 9), 0)
    if (scale != 0) scale <- exp(coef[scale])
    else scale <- 1
    tie.mode <- match("tie.mode", substring(names(coef), 1, 8), 0)
    if (tie.mode != 0) tie.mode <- coef["tie.mode"]
    delta <- coef[match("tie.max", substring(names(coef), 1, 7))]
    ## first player is at home
    weight1 <- plogis(tie.mode)
    weight2 <- 1 - weight1
    ## plogis = expit
    plogis(delta - scale * (weight1 * log(weight1) + weight2 * log(weight2)) +
           scale * (weight1 * log(p) + weight2 * log(1-p)))
}

#tmp <- eval(substitute(player1), data, parent.frame())


#' Plot Proportions of Tied Matches and Non-tied Matches Won
#' 
#' Plot proportions of tied matches and non-tied matches won by the first
#' player, within matches binned by the relative player ability, as expressed
#' by the probability that the first player wins, given the match is not a tie.
#' Add fitted lines for each set of matches, as given by the generalized
#' Davidson model.
#' 
#' If `home.adv` is specified, the results are re-ordered if necessary so
#' that the home player comes first; any matches played on neutral ground are
#' omitted.
#' 
#' First the probability that the first player wins given that the match is not
#' a tie is computed: \deqn{expit(home.adv + abilities[player1] -
#' abilities[player2])} where `home.adv` and `abilities` are
#' parameters from a generalized Davidson model that have been estimated on the
#' log scale.
#' 
#' The matches are then binned according to this probability, grouping together
#' matches with similar relative ability between the first player and the
#' second player. Within each bin, the proportion of tied matches is computed
#' and these proportions are plotted against the mid-point of the bin. Then the
#' bins are re-computed omitting the tied games and the proportion of non-tied
#' matches won by the first player is found and plotted against the new
#' mid-point.
#' 
#' Finally curves are added for the probability of a tie and the conditional
#' probability of win given the match is not a tie, under a generalized
#' Davidson model with parameters as specified by `tie.max`,
#' `tie.scale` and `tie.mode`.
#' 
#' The function can also be used to plot the proportions of wins along with the
#' fitted probability of a win under the Bradley-Terry model.
#' 
#' @param win a logical vector: `TRUE` if player1 wins, `FALSE`
#' otherwise.
#' @param tie a logical vector: `TRUE` if the outcome is a tie,
#' `FALSE` otherwise (`NULL` if there are no ties).
#' @param loss a logical vector: `TRUE` if player1 loses, `FALSE`
#' otherwise.
#' @param player1 an ID factor specifying the first player in each contest,
#' with the same set of levels as `player2`.
#' @param player2 an ID factor specifying the second player in each contest,
#' with the same set of levels as `player2`.
#' @param abilities the fitted abilities from a generalized Davidson model (or
#' a Bradley-Terry model).
#' @param home.adv if applicable, the fitted home advantage parameter from a
#' generalized Davidson model (or a Bradley-Terry model).
#' @param tie.max the fitted parameter from a generalized Davidson model
#' corresponding to the maximum tie probability.
#' @param tie.scale if applicable, the fitted parameter from a generalized
#' Davidson model corresponding to the scale of dependence of the tie
#' probability on the probability that `player1` wins, given the outcome
#' is not a draw.
#' @param tie.mode if applicable, the fitted parameter from a generalized
#' Davidson model corresponding to the location of maximum tie probability, in
#' terms of the probability that `player1` wins, given the outcome is not
#' a draw.
#' @param at.home1 a logical vector: `TRUE` if `player1` is at home,
#' `FALSE` otherwise.
#' @param at.home2 a logical vector: `TRUE` if `player2` is at home,
#' `FALSE` otherwise.
#' @param data an optional data frame providing variables required by the
#' model, with one observation per match.
#' @param subset an optional logical or numeric vector specifying a subset of
#' observations to include in the plot.
#' @param bin.size the approximate number of matches in each bin.
#' @param xlab the label to use for the x-axis.
#' @param ylab the label to use for the y-axis.
#' @param legend text to use for the legend.
#' @param col a vector specifying colours to use for the proportion of non-tied
#' matches won and the proportion of tied matches.
#' @param \dots further arguments passed to plot.
#' @return A list of data frames: \item{win}{ a data frame comprising
#' `prop.win`, the proportion of non-tied matches won by the first player
#' in each bin and `bin.win`, the mid-point of each bin. } \item{tie}{
#' (when ties are present) a data frame comprising `prop.tie`, the
#' proportion of tied matches in each bin and `bin.tie`, the mid-point of
#' each bin. }
#' @note This function is designed for single match outcomes, therefore data
#' aggregated over player pairs will need to be expanded.
#' @author Heather Turner
#' @seealso [GenDavidson()], [BTm()]
#' @keywords models nonlinear
#' @examples
#' 
#' #### A Bradley-Terry example using icehockey data
#' 
#' ## Fit the standard Bradley-Terry model, ignoring home advantage
#' standardBT <- BTm(outcome = result,
#'                   player1 = visitor, player2 = opponent,
#'                   id = "team", data = icehockey)
#' 
#' ## comparing teams on a "level playing field"
#' levelBT <- BTm(result,
#'                data.frame(team = visitor, home.ice = 0),
#'                data.frame(team = opponent, home.ice = home.ice),
#'                ~ team + home.ice,
#'                id = "team", data = icehockey)
#' 
#' ## compare fit to observed proportion won
#' ## exclude tied matches as not explicitly modelled here
#' par(mfrow = c(1, 2))
#' plotProportions(win = result == 1, loss = result == 0,
#'                 player1 = visitor, player2 = opponent,
#'                 abilities = BTabilities(standardBT)[,1],
#'                 data = icehockey, subset = result != 0.5,
#'                 main = "Without home advantage")
#' 
#' plotProportions(win = result == 1, loss = result == 0,
#'                 player1 = visitor, player2 = opponent,
#'                 home.adv = coef(levelBT)["home.ice"],
#'                 at.home1 = 0, at.home2 = home.ice,
#'                 abilities = BTabilities(levelBT)[,1],
#'                 data = icehockey, subset = result != 0.5,
#'                 main = "With home advantage")
#' 
#' #### A generalized Davidson example using football data
#' if (require(gnm)) {
#' 
#'     ## subset to first and last season for illustration
#'     football <- subset(football, season %in% c("2008-9", "2012-13"))
#' 
#'     ## convert to trinomial counts
#'     football.tri <- expandCategorical(football, "result", idvar = "match")
#' 
#'     ## add variable to indicate whether team playing at home
#'     football.tri$at.home <- !logical(nrow(football.tri))
#' 
#'     ## fit Davidson model
#'     Dav <- gnm(count ~ GenDavidson(result == 1, result == 0, result == -1,
#'                                    home:season, away:season, home.adv = ~1,
#'                                    tie.max = ~1,
#'                                    at.home1 = at.home,
#'                                    at.home2 = !at.home) - 1,
#'                eliminate = match, family = poisson, data = football.tri)
#' 
#'     ## fit shifted & scaled Davidson model
#'     shifScalDav <- gnm(count ~
#'         GenDavidson(result == 1, result == 0, result == -1,
#'                     home:season, away:season, home.adv = ~1,
#'                     tie.max = ~1, tie.scale = ~1, tie.mode = ~1,
#'                     at.home1 = at.home,
#'                     at.home2 = !at.home) - 1,
#'         eliminate = match, family = poisson, data = football.tri)
#' 
#'     ## diagnostic plots
#'     main <- c("Davidson", "Shifted & Scaled Davidson")
#'     mod <- list(Dav, shifScalDav)
#'     names(mod) <- main
#'     alpha <- names(coef(Dav)[-(1:2)])
#' 
#'     ## use football.tri data so that at.home can be found,
#'     ## but restrict to actual match results
#'     par(mfrow = c(1,2))
#'     for (i in 1:2) {
#'         coef <- parameters(mod[[i]])
#'         plotProportions(result == 1, result == 0, result == -1,
#'                         home:season, away:season,
#'                         abilities = coef[alpha],
#'                         home.adv = coef["home.adv"],
#'                         tie.max = coef["tie.max"],
#'                         tie.scale = coef["tie.scale"],
#'                         tie.mode = coef["tie.mode"],
#'                         at.home1 = at.home,
#'                         at.home2 = !at.home,
#'                         main = main[i],
#'                         data = football.tri, subset = count == 1)
#'     }
#' }
#' 
#' @importFrom graphics curve plot points
#' @export
plotProportions <- function(win, tie = NULL, loss,
                            player1,
                            player2,
                            abilities = NULL,
                            home.adv = NULL,
                            tie.max = NULL,
                            tie.scale = NULL,
                            tie.mode = NULL,
                            at.home1 = NULL,
                            at.home2 = NULL,
                            data = NULL,
                            subset = NULL,
                            bin.size = 20,
                            xlab = "P(player1 wins | not a tie)",
                            ylab = "Proportion",
                            legend = NULL,
                            col = 1:2,
                            ...){
    call <- as.list(match.call())
    var <- intersect(names(call), c("win", "tie", "loss",
                                    "player1", "player2",
                                    "at.home1", "at.home2"))
    var <- var[!sapply(call[var], is.null)]

    dat <- with(data, do.call("data.frame", call[var]))
    if (!missing(subset)){
        subset <- eval(substitute(subset), data, parent.frame())
        dat <- subset(dat, subset)
    }
    if (!missing(tie) && sum(dat$tie) == 0) dat$tie <- NULL
    if (!is.null(home.adv) && (missing(at.home1) || missing(at.home2)))
        stop("at.home1 and at.home2 must be specified")
    if (!is.null(home.adv)){
        ## exclude neutral contests, make sure home player is first
        dat <- subset(dat, at.home1 | at.home2)
        swap <- which(as.logical(dat$at.home2))
        if (length(swap)) {
            dat$win[swap] <- dat$loss[swap]
            if (is.null(dat$tie)) dat$loss[swap] <- !dat$win[swap]
            else dat$loss[swap] <- !(dat$win[swap] | dat$tie[swap])
            tmp <- dat$player1[swap]
            dat$player1[swap] <- dat$player2[swap]
            dat$player2[swap] <- tmp
            dat$at.home1[swap] <- TRUE
            dat$at.home2[swap] <- FALSE
        }
    } else home.adv <- 0
    ### get proportions
    p <- with(dat, plogis(home.adv + abilities[as.character(player1)] -
                          abilities[as.character(player2)]))
    ## Depending on the distribution of p_ij (across all matches),
    ## divide the range of probabilities p_ij into discrete "bins", each
    ## of which has at least (say) 20 matches in it
    getBins <- function(p, bin.size) {
        ## alternatively estimate bins to same size intervals
        ## at least bin.size - distribute extra evenly over range
        min.size <- bin.size
        n <- length(p)
        r <- n %% min.size
        size <- rep(min.size, n %/% min.size)
        if (r > 0) {
            step <- length(size)/r
            extra <- round(seq(from = step/2 + 0.01,
                               to = step/2 + 0.01 + (r - 1)*step, by = step))
            size[extra] <- min.size + 1
        }
        bin <- factor(rep(seq(length(size)), size))[match(p, sort(p))]
        low <- sort(p)[cumsum(c(1, size[-length(size)]))] #first
        high <- sort(p)[cumsum(size)] #last
        mid <- (high - low)/2 + low
        list(bin = bin, mid = mid)
    }
    winBin <- getBins(p, bin.size)
    ## Within each bin b, calculate
    ## d_b = proportion of matches in that bin that were drawn
    if (!is.null(dat$tie)) {
        tieBin <- winBin
        tri <- with(dat, win - (!win & !tie))
        d_b <- tapply(tri, tieBin$bin, function(x) sum(x == 0)/length(x))
        ## recompute bins omitting ties
        winBin <- getBins(p[!dat$tie], bin.size)
    }

    ## h_b = proportion of *non-drawn* matches in that bin that were won
    ## by the home team
    if (!is.null(dat$tie)) {
        h_b <- tapply(tri[!dat$tie], winBin$bin,
                      function(x) sum(x == 1)/length(x))
    }
    else h_b <- tapply(dat$win, winBin$bin, function(x) sum(x == 1)/length(x))

    ## Plot d_b and h_b against the bin midpoints, in a plot with
    ## axis limits both (0,1)
    plot(h_b ~ winBin$mid, xlim = c(0, 1), ylim = c(0, 1),
         xlab = xlab, ylab = ylab, ...)
    if (missing(legend)) {
        if (is.null(dat$tie)) legend <- "Matches won"
        else legend <- c("Non-tied matches won", "Matches tied")
    }
    legend("topleft", legend, col = col[c(1, 2[!missing(tie)])],
           pch = 1)
    if (!is.null(dat$tie)) points(d_b ~ tieBin$mid, col = col[2])

    ## Add to the plot the lines/curves
    ## y = x
    ## y = expit(log(nu * sqrt(p_ij * (1 - p_ij))))
    ## The d_b should lie around the latter curve, and the h_b should
    ## lie around the former line.  Any clear patterns of departure are
    ## of interest.
    curve(I, 0, 1, add = TRUE)

    env <- new.env()
    environment(GenDavidsonTie) <- env
    coef <- na.omit(c(home.adv = unname(home.adv),
                      tie.max = unname(tie.max),
                      tie.scale = unname(tie.scale),
                      tie.mode = unname(tie.mode)))
    assign("coef", coef, envir=env)
    curve(GenDavidsonTie, 0, 1, col = col[2], add = TRUE)
    out <- list(win = data.frame(prop.win = h_b, bin.win = winBin$mid))
    if (!is.null(dat$tie))
        out <- c(out, tie = data.frame(prop.tie = d_b, bin.tie = tieBin$mid))
    invisible(out)
}
