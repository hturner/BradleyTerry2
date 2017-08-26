#' Control Aspects of the glmmPQL Algorithm
#' 
#' Set control variables for the glmmPQL algorithm.
#' 
#' This function provides an interface to control the PQL algorithm used by
#' [BTm()] for fitting Bradley Terry models with random effects.
#' 
#' The algorithm iterates between a series of iterated weighted least squares
#' iterations to update the fixed effects and a single Fisher scoring iteration
#' to update the standard deviation of the random effects.
#' 
#' Convergence of both the inner and outer iterations are judged by comparing
#' the squared components of the relevant score vector with corresponding
#' elements of the diagonal of the Fisher information matrix. If, for all
#' components of the relevant score vector, the ratio is less than
#' `tolerance^2`, or the corresponding diagonal element of the Fisher
#' information matrix is less than 1e-20, iterations cease.
#' 
#' @param maxiter the maximum number of outer iterations.
#' @param IWLSiter the maximum number of iterated weighted least squares
#' iterations used to estimate the fixed effects, given the standard deviation
#' of the random effects.
#' @param tol the tolerance used to determine convergence in the IWLS
#' iterations and over all (see details).
#' @param trace logical: whether or not to print the score for the random
#' effects variance at the end of each iteration.
#' @return A list with the arguments as components.
#' @author Heather Turner
#' @seealso [glmmPQL()], [BTm()]
#' @references Breslow, N. E. and Clayton, D. G. (1993), Approximate inference
#' in Generalized Linear Mixed Models. *Journal of the American
#' Statistical Association* **88**(421), 9--25.
#' @keywords models
#' @examples
#' 
#' ## Variation on example(flatlizards)
#' result <- rep(1, nrow(flatlizards$contests))
#' 
#' ## BTm passes arguments on to glmmPQL.control()
#' args(BTm)
#' BTmodel <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
#'                head.length[..] + SVL[..] + (1|..),
#'                data = flatlizards, tol = 1e-3, trace = TRUE)
#' summary(BTmodel)
#' 
#' @export
glmmPQL.control <- function (maxiter = 50, IWLSiter = 10, tol = 1e-6,
                             trace = FALSE) {
    call <- as.list(match.call())

    if (length(call) > 1) {
        argPos <- match(c("maxiter", "IWLSiter", "tol"), names(call))
        for (n in argPos[!is.na(argPos)]) {
            if (!is.numeric(call[[n]]) || call[[n]] <= 0)
                stop("value of '", names(call)[n], "' must be > 0")
        }
    }

    list(maxiter = maxiter, IWLSiter = IWLSiter, tol = tol,
         trace = trace)
}
