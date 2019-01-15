#' @export
model.matrix.BTm <- function (object, ...){
    object$contrasts <- NULL
    NextMethod()
}
