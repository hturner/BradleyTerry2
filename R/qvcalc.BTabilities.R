#' Quasi Variances for Estimated Abilities
#' 
#' A method for [qvcalc::qvcalc()] to compute a set of quasi variances (and 
#' corresponding quasi standard errors) for estimated abilities from a 
#' Bradley-Terry model as returned by [BTabilities()].
#' 
#' For details of the method see Firth (2000), Firth (2003) or Firth and de 
#' Menezes (2004). Quasi variances generalize and improve the accuracy of
#' \dQuote{floating absolute risk} (Easton et al., 1991). This device for 
#' economical model summary was first suggested by Ridout (1989).
#' 
#' Ordinarily the quasi variances are positive and so their square roots
#' (the quasi standard errors) exist and can be used in plots, etc.
#' 
#' @param object a `"BTabilities"` object as returned by [BTabilities()].
#' @param ... additional arguments, currently ignored.
#' @return A list of class `"qv"`, with components
#' \item{covmat}{The full variance-covariance matrix for the estimated 
#' abilities.}
#' \item{qvframe}{A data frame with variables `estimate`, `SE`, `quasiSE` and 
#' `quasiVar`, the last two being a quasi standard error and quasi-variance
#' for each ability.}
#' \item{dispersion}{`NULL` (dispersion is fixed to 1).}
#' \item{relerrs}{Relative errors for approximating the standard errors of all 
#' simple contrasts.}
#' \item{factorname}{The name of the ID factor identifying players in the `BTm`
#' formula.}
#' \item{coef.indices}{`NULL` (no required for this method).}
#' \item{modelcall}{The call to `BTm` to fit the Bradley-Terry model from which
#' the abilities were estimated.}
#' @references
#' Easton, D. F, Peto, J. and Babiker, A. G. A. G. (1991) Floating absolute 
#' risk: an alternative to relative risk in survival and case-control analysis 
#' avoiding an arbitrary reference group. *Statistics in Medicine* **10**, 
#' 1025--1035.
#' 
#' Firth, D. (2000)  Quasi-variances in Xlisp-Stat and on the web.  
#' *Journal of Statistical Software* **5(4)**, 1--13.  
#' \doi{https://doi.org/10.18637/jss.v005.i04}.
#' 
#' Firth, D. (2003) Overcoming the reference category problem in the 
#' presentation of statistical models. *Sociological Methodology*
#' **33**, 1--18.
#' 
#' Firth, D. and de Menezes, R. X. (2004)  Quasi-variances.  
#' *Biometrika* **91**, 65--80.  
#' 
#' Menezes, R. X. de (1999)  More useful standard errors for group and factor
#' effects in generalized linear models.  *D.Phil. Thesis*,
#' Department of Statistics, University of Oxford.
#' 
#' Ridout, M.S. (1989). Summarizing the results of fitting generalized
#' linear models to data from designed experiments. In: *Statistical
#'     Modelling: Proceedings of GLIM89 and the 4th International
#'     Workshop on Statistical Modelling held in Trento, Italy, July 17--21,
#'     1989* (A. Decarli et al., eds.), pp 262--269. New York: Springer.
#' @author David Firth
#' @seealso [qvcalc::worstErrors()], [qvcalc::plot.qv()].  
#' @examples     
#' example(baseball)
#' baseball.qv <- qvcalc(BTabilities(baseballModel2))
#' print(baseball.qv)
#' plot(baseball.qv, xlab = "team",
#'      levelNames = c("Bal", "Bos", "Cle", "Det", "Mil", "NY", "Tor"))
#' @method qvcalc BTabilities
#' @importFrom qvcalc qvcalc.default
#' @importFrom stats coef vcov
#' @export
qvcalc.BTabilities <- function(object, ...){
    vc <- vcov(object)
    cf <- coef(object)
    factorname <- attr(object, "factorname")
    modelcall <- attr(object, "modelcall")
    qvcalc.default(vc,
                   factorname = factorname,
                   estimates = cf,
                   modelcall = modelcall)
}

#' @importFrom qvcalc qvcalc
#' @export
qvcalc::qvcalc
