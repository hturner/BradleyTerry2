vcov.glmmPQL <- function (object, ...)
{
    so <- summary.glmmPQL(object, corr = FALSE, ...)
    so$dispersion * so$cov.unscaled
}
