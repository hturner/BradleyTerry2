augment.BTm <- function(x, ...){
    dots <- list(...)
    if ("level" %in% names(dots)){
        # reduce to fixed effects because level not passed on by augment.glm
        if (dots$level == 0){
            x$random <- NULL
            x$sigma <- 0
        }
    }
    NextMethod(x = x, ...)
}