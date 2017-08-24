#' Predict Method for BTglmmPQL Objects
#' 
#' Obtain predictions and optionally standard errors of those predictions from
#' a `"BTglmmPQL"` object.
#' 
#' If `newdata` is omitted the predictions are based on the data used for
#' the fit.  In that case how cases with missing values in the original fit are
#' treated is determined by the `na.action` argument of that fit.  If
#' `na.action = na.omit` omitted cases will not appear in the residuals,
#' whereas if `na.action = na.exclude` they will appear (in predictions
#' and standard errors), with residual value `NA`.  See also
#' `napredict`.
#' 
#' Standard errors for the predictions are approximated assuming the variance
#' of the random effects is known, see Booth and Hobert (1998).
#' 
#' @param object a fitted object of class `"BTglmmPQL"`
#' @param newdata (optional) a data frame in which to look for variables with
#' which to predict.  If omitted, the fitted linear predictors are used.
#' @param newrandom if `newdata` is provided, a corresponding design
#' matrix for the random effects, will columns corresponding to the random
#' effects estimated in the original model.
#' @param level an integer vector giving the level(s) at which predictions are
#' required. Level zero corresponds to population-level predictions (fixed
#' effects only), whilst level one corresponds to the individual-level
#' predictions (full model) which are NA for contests involving individuals not
#' in the original data.
#' @param type the type of prediction required.  The default is on the scale of
#' the linear predictors; the alternative `"response"` is on the scale of
#' the response variable. Thus for a default binomial model the default
#' predictions are of log-odds (probabilities on logit scale) and `type =
#' "response"` gives the predicted probabilities. The `"terms"` option
#' returns a matrix giving the fitted values of each term in the model formula
#' on the linear predictor scale (fixed effects only).
#' @param se.fit logical switch indicating if standard errors are required.
#' @param terms with `type ="terms"` by default all terms are returned.  A
#' character vector specifies which terms are to be returned.
#' @param na.action function determining what should be done with missing
#' values in `newdata`.  The default is to predict `NA`.
#' @param \dots further arguments passed to or from other methods.
#' @return If `se.fit = FALSE`, a vector or matrix of predictions.  If
#' `se = TRUE`, a list with components \item{fit }{Predictions}
#' \item{se.fit }{Estimated standard errors}
#' @author Heather Turner
#' @seealso [predict.glm()], [predict.BTm()]
#' @references Booth, J. G. and Hobert, J. P. (1998). Standard errors of
#' prediction in Generalized Linear Mixed Models. *Journal of the American
#' Statistical Association* **93**(441), 262 -- 272.
#' @keywords models
#' @examples
#' 
#' attach(seeds)
#' 
#' seedsModel <- glmmPQL(cbind(r, n - r) ~ seed + extract,
#'                       random = diag(length(r)),
#'                       family = binomial)
#' 
#' pred <- predict(seedsModel, level = 0)
#' predTerms <- predict(seedsModel, type = "terms")
#' 
#' all.equal(pred, rowSums(predTerms) + attr(predTerms, "constant"))
#' 
#' @export
predict.BTglmmPQL <- function (object, newdata = NULL, newrandom = NULL,
                             level = 1, type = c("link", "response", "terms"),
                             se.fit = FALSE, terms = NULL,
                             na.action = na.pass, ...) {
    ## only pass on if a glm
    if (object$sigma == 0) {
        if (level != 0) warning("Fixed effects model: setting level to 0")
        return(NextMethod())
    }
    if (!all(level %in% c(0, 1))) stop("Only level %in% c(0, 1) allowed")
    type <- match.arg(type)
    if (!is.null(newdata) || type == "terms") tt <- terms(object)
    if (!is.null(newdata)) {
        ## newdata should give variables in terms formula
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action,
                         xlev = object$xlevels)
        na.action <- attr(m, "na.action")
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        D <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        np <- nrow(D) # n predictions
        offset <- rep(0, np)
        if (!is.null(off.num <- attr(tt, "offset")))
            for (i in off.num) offset <- offset + eval(attr(tt,
                "variables")[[i + 1]], newdata)
        if (!is.null(object$call$offset))
            offset <- offset + eval(object$call$offset, newdata)
    }
    else {
        D <- model.matrix(object)
        newrandom <- object$random
        na.action <- object$na.action
        offset <- object$offset
    }
    if (se.fit == TRUE) {
        sigma <- object$sigma
        w <- sqrt(object$weights)
        if (!is.null(newdata)) {
            wX <- w * model.matrix(object)
            wZ <- w * object$random
        }
        else {
            wX <- w * D
            wZ <- w * newrandom
        }
        XWX <- crossprod(wX)
        XWZ <- crossprod(wX, wZ)
        ZWZ <- crossprod(wZ, wZ)
        diag(ZWZ) <- diag(ZWZ) + 1/sigma^2
        C <- cbind(XWX, XWZ)
        C <- chol(rbind(C, cbind(t(XWZ), ZWZ)))
        if (type == "terms" || level == 0){
            ## work out (chol of inverse of) topleft of C-inv directly
            A <- backsolve(chol(ZWZ), t(XWZ), transpose = TRUE)
            A <- chol(XWX - t(A) %*% A)
        }
    }
    if (type == "terms") { # ignore level
        if (1 %in% level)
            warning("type = \"terms\": setting level to 0", call. = FALSE)
        coef <- coef(object) #fixef
        aa <- attr(D, "assign")
        ll <- attr(tt, "term.labels")
        if (!is.null(terms)) {
            include <- ll %in% terms
            ll <- ll[include]
        }
        hasintercept <- attr(tt, "intercept") > 0L
        if (hasintercept) {
            avx <- colMeans(model.matrix(object))
            termsconst <- sum(avx * coef) #NA coefs?
            D <- sweep(D, 2, avx)
        }
        pred0 <- matrix(ncol = length(ll), nrow = NROW(D))
        colnames(pred0) <- ll
        if (se.fit) {
            A <- chol2inv(A)
            se.pred0 <- pred0
        }
        for (i in seq(length.out = length(ll))){
            ind <- aa == which(attr(tt, "term.labels") == ll[i])
            pred0[, i] <- D[, ind, drop = FALSE] %*% coef[ind]
            if (se.fit) {
                se.pred0[, i] <-  sqrt(diag(D[, ind] %*%
                                            tcrossprod(A[ind, ind], D[, ind])))
            }
        }
        if (hasintercept) attr(pred0, "constant") <- termsconst
        if (se.fit) return(list(fit = pred0, se.fit = se.pred0))
        return(pred0)
    }
    if (0 %in% level) {
        pred0 <- napredict(na.action, c(D %*% coef(object)) + offset)
        if (type == "response")
            pred0 <- family(object)$linkinv(pred0)
        if (se.fit == TRUE) {
            na.act <- attr(na.exclude(pred0), "na.action")
            H <- backsolve(A, t(na.exclude(D)), transpose = TRUE)
            ## se.pred0 <- sqrt(diag(D %*% chol2inv(C)[1:ncol(D), 1:ncol(D)] %*% t(D)))
            se.pred0 <- napredict(na.action, napredict(na.act, sqrt(colSums(H^2))))
           if (type == "response")
               se.pred0 <- se.pred0*abs(family(object)$mu.eta(pred0))
            pred0 <- list(fit = pred0, se.fit = se.pred0)
        }
        if (identical(level, 0)) return(pred0)
    }

    r <- nrow(D)
    ## newrandom should give new design matrix for original random effects
    if (!is.null(newdata)){
        if(is.null(newrandom))
            stop("newdata specified without newrandom")
        if (!is.null(na.action))
            newrandom <- newrandom[-na.action, , drop = FALSE]
    }
    if (!identical(dim(newrandom), c(r, ncol(object$random))))
        stop("newrandom should have ", r, " rows and ",
             ncol(object$random), " columns")
    D <- cbind(D, newrandom)
    coef <- c(coef(object), attr(coef(object), "random"))
    pred <- napredict(na.action, c(D %*% coef) + offset)
    if (type == "response")
        pred <- family(object)$linkinv(pred)
    if (se.fit == TRUE) {
        ##se.pred <- sqrt(diag(D %*% chol2inv(C) %*% t(D)))
        na.act <- attr(na.exclude(pred), "na.action")
        H <- backsolve(C, t(na.exclude(D)), transpose = TRUE)
        se.pred <- napredict(na.action, napredict(na.act, sqrt(colSums(H^2))))
        if (type == "response")
            se.pred <- se.pred*abs(family(object)$mu.eta(pred))
        pred <- list(fit = pred, se.fit = se.pred)
    }

    if (0 %in% level)
        list(population = pred0, individual = pred)
    else pred
}
