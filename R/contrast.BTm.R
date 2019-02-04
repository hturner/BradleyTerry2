contrast.BTm <- function(fit, a, b, ...){
    len_a <- lengths(a)
    len_b <- lengths(b)
    a <- rep(a, each = len_b)
    b <- rep(b, len_a)
    contr <- lapply(seq(length(a)), function(i) {
        BTcontrasts(fit, group1 = a[[i]], group2 = b[[i]])})
    contr <- do.call("rbind", contr)
    fit$formula <- formula(terms(fit))
    fit$contrasts <- NULL
    fit$xlevels <- NULL
    class(fit) <- class(fit)[-1]
    dots <- list(...)
    dots$a <- as.list(as.data.frame(contr))
    dots$b <- NULL
    do.call("contrast", c(list(fit), dots))
}