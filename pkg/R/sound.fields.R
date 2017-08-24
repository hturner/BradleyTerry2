#' Kousgaard (1984) Data on Pair Comparisons of Sound Fields
#' 
#' The results of a series of factorial subjective room acoustic experiments
#' carried out at the Technical University of Denmark by A C Gade.
#' 
#' The variables `win1.adj` and `win2.adj` are provided in order to
#' allow a simple way of handling ties (in which a tie counts as half a win and
#' half a loss), which is slightly different numerically from the Davidson
#' (1970) method that is used by Kousgaard (1984): see the examples.
#' 
#' @name sound.fields
#' @docType data
#' @format A list containing two data frames, `sound.fields$comparisons`,
#' and `sound.fields$design`.
#' 
#' The `sound.fields$comparisons` data frame has 84 observations on the
#' following 8 variables: \describe{ 
#' \item{field1}{a factor with levels
#' `c("000", "001", "010", "011", "100", "101", "110", "111")`, the first
#' sound field in a comparison} 
#' \item{field2}{a factor with the same
#' levels as `field1`; the second sound field in a comparison}
#' \item{win1}{integer, the number of times that `field1` was
#' preferred to `field2`} 
#' \item{tie}{integer, the number of times
#' that no preference was expressed when comparing `field1` and
#' `field2`} 
#' \item{win2}{integer, the number of times that
#' `field2` was preferred to `field1`}
#' \item{win1.adj}{numeric, equal to `win1 + tie/2`}
#' \item{win2.adj}{numeric, equal to `win2 + tie/2`}
#' \item{instrument}{a factor with 3 levels, `c("cello", "flute",
#' "violin")`} }
#' 
#' The `sound.fields$design` data frame has 8 observations (one for each
#' of the sound fields compared in the experiment) on the following 3
#' variables: \describe{ 
#' \item{a")}{a factor with levels `c("0",
#' "1")`, the *direct sound* factor (0 for *obstructed sight line*, 1
#' for *free sight line*); contrasts are sum contrasts} 
#' \item{b}{a
#' factor with levels `c("0", "1")`, the *reflection* factor (0 for
#' *-26dB*, 1 for *-20dB*); contrasts are sum contrasts}
#' \item{c}{a factor with levels `c("0", "1")`, the
#' *reverberation* factor (0 for *-24dB*, 1 for *-20dB*);
#' contrasts are sum contrasts} }
#' @author David Firth
#' @references Davidson, R. R. (1970) Extending the Bradley-Terry model to
#' accommodate ties in paired comparison experiments.  *Journal of the
#' American Statistical Association* **65**, 317--328.
#' @source Kousgaard, N. (1984) Analysis of a Sound Field Experiment by a Model
#' for Paired Comparisons with Explanatory Variables.  *Scandinavian
#' Journal of Statistics* **11**, 51--57.
#' @keywords datasets
#' @examples
#' 
#' ##
#' ##  Fit the Bradley-Terry model to data for flutes, using the simple 'add 0.5'
#' ##  method to handle ties:
#' ##
#' flutes.model <- BTm(cbind(win1.adj, win2.adj), field1, field2, ~ field,
#'                     id = "field",
#'                     subset = (instrument == "flute"),
#'                     data = sound.fields)
#' ##
#' ##  This agrees (after re-scaling) quite closely with the estimates given
#' ##  in Table 3 of Kousgaard (1984):
#' ##
#' table3.flutes <- c(-0.581, -1.039, 0.347, 0.205, 0.276, 0.347, 0.311, 0.135)
#' plot(c(0, coef(flutes.model)), table3.flutes)
#' abline(lm(table3.flutes ~ c(0, coef(flutes.model))))
#' ##
#' ##  Now re-parameterise that model in terms of the factorial effects, as
#' ##  in Table 5 of Kousgaard (1984):
#' ##
#' flutes.model.reparam <- update(flutes.model,
#'                                formula = ~ a[field] * b[field] * c[field]
#' 			       )
#' table5.flutes <- c(.267, .250, -.088, -.294, .062, .009, -0.070)
#' plot(coef(flutes.model.reparam), table5.flutes)
#' abline(lm(table5.flutes ~ coef(flutes.model.reparam)))
#' 
"sound.fields"
