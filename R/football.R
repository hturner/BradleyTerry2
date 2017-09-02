#' English Premier League Football Results 2008/9 to 2012/13
#' 
#' The win/lose/draw results for five seasons of the English Premier League
#' football results, from 2008/9 to 2012/13
#' 
#' In each season, there are 20 teams, each of which plays one home game and
#' one away game against all the other teams in the league. The results in 380
#' games per season.
#' 
#' @name football
#' @docType data
#' @format A data frame with 1881 observations on the following 4 variables.
#' \describe{ 
#' \item{season}{a factor with levels `2008-9`,
#' `2009-10`, `2010-11`, `2011-12`, `2012-13`}
#' \item{home}{a factor specifying the home team, with 29 levels
#' `Ars` (Arsenal), ... , `Wol` (Wolverhampton)}
#' \item{away}{a factor specifying the away team, with the same levels
#' as `home`.} 
#' \item{result}{a numeric vector giving the result
#' for the home team: 1 for a win, 0 for a draw, -1 for a loss.} }
#' @seealso [GenDavidson()]
#' @references Davidson, R. R. (1970). On extending the Bradley-Terry model to
#' accommodate ties in paired comparison experiments. *Journal of the
#' American Statistical Association*, **65**, 317--328.
#' @source These data were downloaded from http://soccernet.espn.go.com in
#' 2013. The site has since moved and the new site does not appear to have an
#' equivalent source.
#' @keywords datasets
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
#'     ### fit Davidson model for ties
#'     ###  - subset to first and last season for illustration
#'     Davidson <- gnm(count ~
#'         GenDavidson(result == 1, result == 0, result == -1,
#'                     home:season, away:season,
#'                     home.adv = ~1, tie.max = ~1,
#'                     at.home1 = at.home, at.home2 = !at.home) - 1,
#'         eliminate = match, family = poisson, data = football.tri,
#'         subset = season %in% c("2008-9", "2012-13"))
#' 
#'     ### see ?GenDavidson for further analysis
#' }
#' 
"football"
