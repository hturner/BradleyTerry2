#' Dittrich, Hatzinger and Katzenbeisser (1998, 2001) Data on Management School
#' Preference in Europe
#' 
#' \emph{Community of European management schools} (CEMS) data as used in the
#' paper by Dittrich et al. (1998, 2001), re-formatted for use with
#' \code{\link{BTm}}
#' 
#' The variables \code{win1.adj} and \code{win2.adj} are provided in order to
#' allow a simple way of handling ties (in which a tie counts as half a win and
#' half a loss), which is slightly different numerically from the Davidson
#' (1970) method that is used by Dittrich et al. (1998): see the examples.
#' 
#' @name CEMS
#' @docType data
#' @format A list containing three data frames, \code{CEMS$preferences},
#' \code{CEMS$students} and \code{CEMS$schools}.
#' 
#' The \code{CEMS$preferences} data frame has \code{303 * 15 = 4505}
#' observations (15 possible comparisons, for each of 303 students) on the
#' following 8 variables: \describe{ 
#' \item{student}{a factor with
#' levels \code{1:303}} 
#' \item{school1}{a factor with levels
#' \code{c("Barcelona", "London", "Milano", "Paris", "St.Gallen",
#' "Stockholm")}; the first management school in a comparison}
#' \item{school2}{a factor with the same levels as \code{school1}; the
#' second management school in a comparison} 
#' \item{win1}{integer (value
#' 0 or 1) indicating whether \code{school1} was preferred to \code{school2}}
#' \item{win2}{integer (value 0 or 1) indicating whether \code{school2}
#' was preferred to \code{school1}} 
#' \item{tied}{integer (value 0 or 1)
#' indicating whether no preference was expressed}
#' \item{win1.adj}{numeric, equal to \code{win1 + tied/2}}
#' \item{win2.adj}{numeric, equal to \code{win2 + tied/2}} }
#' 
#' The \code{CEMS$students} data frame has 303 observations (one for each
#' student) on the following 8 variables: \describe{ 
#' \item{STUD}{a
#' factor with levels \code{c("other", "commerce")}, the student's main
#' discipline of study} 
#' \item{ENG}{a factor with levels \code{c("good,
#' poor")}, indicating the student's knowledge of English} 
#' \item{FRA}{a
#' factor with levels \code{c("good, poor")}, indicating the student's
#' knowledge of French} 
#' \item{SPA}{a factor with levels \code{c("good,
#' poor")}, indicating the student's knowledge of Spanish}
#'  \item{ITA}{a
#' factor with levels \code{c("good, poor")}, indicating the student's
#' knowledge of Italian} 
#' \item{WOR}{a factor with levels \code{c("no",
#' "yes")}, whether the student was in full-time employment while studying}
#' \item{DEG}{a factor with levels \code{c("no", "yes")}, whether the
#' student intended to take an international degree} 
#' \item{SEX}{a
#' factor with levels \code{c("female", "male")} } }
#' 
#' The \code{CEMS$schools} data frame has 6 observations (one for each
#' management school) on the following 7 variables: \describe{
#' \item{Barcelona}{numeric (value 0 or 1)}
#' \item{London}{numeric (value 0 or 1)} 
#' \item{Milano}{numeric
#' (value 0 or 1)} \item{Paris}{numeric (value 0 or 1)}
#' \item{St.Gallen}{numeric (value 0 or 1)}
#' \item{Stockholm}{numeric (value 0 or 1)} 
#' \item{LAT}{numeric
#' (value 0 or 1) indicating a 'Latin' city} }
#' @author David Firth
#' @references Davidson, R. R. (1970) Extending the Bradley-Terry model to
#' accommodate ties in paired comparison experiments.  \emph{Journal of the
#' American Statistical Association} \bold{65}, 317--328.
#' 
#' Dittrich, R., Hatzinger, R. and Katzenbeisser, W. (1998) Modelling the
#' effect of subject-specific covariates in paired comparison studies with an
#' application to university rankings.  \emph{Applied Statistics} \bold{47},
#' 511--525.
#' 
#' Dittrich, R., Hatzinger, R. and Katzenbeisser, W. (2001) Corrigendum:
#' Modelling the effect of subject-specific covariates in paired comparison
#' studies with an application to university rankings. \emph{Applied
#' Statistics} \bold{50}, 247--249.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  \emph{Journal of Statistical Software}, \bold{48}(9), 1--21.
#' @source Royal Statistical Society datasets website, at
#' \url{http://onlinelibrary.wiley.com/journal/10.1111/(ISSN)1467-9876/homepage/47_4.htm}.
#' @keywords datasets
#' @examples
#' 
#' ##
#' ##  Fit the standard Bradley-Terry model, using the simple 'add 0.5'
#' ##  method to handle ties:
#' ##
#' table3.model <-  BTm(outcome = cbind(win1.adj, win2.adj),
#'                      player1 = school1, player2 = school2,
#'                      formula = ~.. , refcat = "Stockholm",
#'                      data = CEMS)
#' ##  The results in Table 3 of Dittrich et al (2001) are reproduced
#' ##  approximately by a simple re-scaling of the estimates:
#' table3 <- summary(table3.model)$coef[, 1:2]/1.75
#' print(table3)
#' ##
#' ##  Now fit the 'final model' from Table 6 of Dittrich et al.:
#' ##
#' table6.model <-  BTm(outcome = cbind(win1.adj, win2.adj),
#'                      player1 = school1, player2 = school2,
#'                      formula = ~ .. +
#'                          WOR[student] * Paris[..] +
#'                          WOR[student] * Milano[..] +
#'                          WOR[student] * Barcelona[..] +
#'                          DEG[student] * St.Gallen[..] +
#'                          STUD[student] * Paris[..] +
#'                          STUD[student] * St.Gallen[..] +
#'                          ENG[student] * St.Gallen[..] +
#'                          FRA[student] * London[..] +
#'                          FRA[student] * Paris[..] +
#'                          SPA[student] * Barcelona[..] +
#'                          ITA[student] * London[..] +
#'                          ITA[student] * Milano[..] +
#'                          SEX[student] * Milano[..],
#'                      refcat = "Stockholm",
#'                      data = CEMS)
#' ##
#' ##  Again re-scale to reproduce approximately Table 6 of Dittrich et
#' ##  al. (2001):
#' ##
#' table6 <- summary(table6.model)$coef[, 1:2]/1.75
#' print(table6)
#' ##
#' \dontrun{
#' ##  Now the slightly simplified model of Table 8 of Dittrich et al. (2001):
#' ##
#' table8.model <-  BTm(outcome = cbind(win1.adj, win2.adj),
#'                      player1 = school1, player2 = school2,
#'                      formula = ~ .. +
#'                          WOR[student] * LAT[..] +
#'                          DEG[student] * St.Gallen[..] +
#'                          STUD[student] * Paris[..] +
#'                          STUD[student] * St.Gallen[..] +
#'                          ENG[student] * St.Gallen[..] +
#'                          FRA[student] * London[..] +
#'                          FRA[student] * Paris[..] +
#'                          SPA[student] * Barcelona[..] +
#'                          ITA[student] * London[..] +
#'                          ITA[student] * Milano[..] +
#'                          SEX[student] * Milano[..],
#'                      refcat = "Stockholm",
#'                      data = CEMS)
#' table8 <- summary(table8.model)$coef[, 1:2]/1.75
#' ##
#' ##  Notice some larger than expected discrepancies here (the coefficients
#' ##  named "..Barcelona", "..Milano" and "..Paris") from the results in
#' ##  Dittrich et al. (2001).  Apparently a mistake was made in Table 8 of
#' ##  the published Corrigendum note (R. Dittrich personal communication,
#' ##  February 2010).
#' ##
#' print(table8)
#' }
#' 
"CEMS"