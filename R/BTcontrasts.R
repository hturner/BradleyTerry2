# this can be used with e.g. car::lht; multcomp::glht; gmodels::estimable
BTcontrasts <- function(object, group1, group2, label = "grp1 v grp2"){
    n <- length(group1)
    # set up contrast as if had full coef/vcov
    nm <- object$xlevels[[object$id]]
    res <- matrix(0.0, 1, length(nm), dimnames = list(label, nm))
    for (i in seq(n)){
        res[1, group1] <- 1/length(group1)
        res[1, group2] <- -1/length(group2)
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
