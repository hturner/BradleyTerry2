#' @method qvcalc BTabilities
#' @importFrom stats coef vcov
#' @export
qvcalc.BTabilities <- function(object, ...){
    vc <- vcov(object)
    cf <- coef(object)
    factorname <- attr(object, "factorname")
    modelcall <- attr(object, "modelcall")
    NextMethod(vc,
               factorname = factorname,
               estimates = cf,
               modelcall = modelcall)
}

#' @importFrom qvcalc qvcalc
#' @export
qvcalc::qvcalc
