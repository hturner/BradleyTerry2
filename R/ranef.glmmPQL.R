ranef.glmmPQL <- function(object, ...){
    attr(object$coefficients, "random")
}
