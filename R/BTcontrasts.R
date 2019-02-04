# this can be used with e.g. car::lht; multcomp::glht; gmodels::estimable
# use similar design to contrast::contrast for potential future extension
BTcontrasts <- function(object, group1, group2, label = "grp1 v grp2"){
    # replicate groups to match lengths
    len_a <- lengths(group1)
    len_b <- lengths(group2)
    a <- rep(group1, each = len_b)
    b <- rep(group2, len_a)
    n <- length(a)
    # set up contrast as if had full coef/vcov
    nm <- object$xlevels[[object$id]]
    res <- matrix(0.0, n, length(nm), dimnames = list(label, nm))
    for (i in seq(n)){
        res[1, a[[i]]] <- 1/length(a[[i]])
        res[1, b[[i]]] <- -1/length(b[[i]])
    }
    # setup factor reflecting contrasts used ..
    fac <- factor(object$xlevels[[object$id]], 
                  levels = object$xlevels[[object$id]],
                  labels = paste0(object$id, object$xlevels[[object$id]]))
    if (!is.null(object$refcat)) {
        fac <- C(relevel(fac, object$refcat),
                 "contr.treatment")
    } else fac <- C(fac, object$contrasts[[object$id]])
    contr <- contrasts(fac)
    colnames(contr) <- names(coef(object))
    res %*% contr
}
