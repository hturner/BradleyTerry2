#' @export
model.matrix.BTm <- function(object, ...){
    model.frame(object)$X
}
