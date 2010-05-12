predict.BTm <- function (object, newdata = NULL, level = 0,
                         type = c("link", "response", "terms"), se.fit = FALSE,
                         dispersion = NULL, terms = NULL,
                         na.action = na.pass, ...) {
    if (!is.null(newdata)) {
        ## need to define X so will work with model terms
        setup <- match(c("player1", "player2", "formula", "id",
                        "separate.ability", "refcat", "weights",
                        "subset", "offset", "contrasts"), names(object$call), 0L)
        setup <- as.call(c(quote(BradleyTerry2:::BTm.setup),
                           as.list(object$call)[setup],
                           list(data = newdata)))
        setup <- eval(setup, environment(object$formula))
        if (ncol(setup$X) != length(fixef(object))){
            ## will be due to separate abilities - else error by now
            X <- matrix(0, nrow(setup$X), length(fixef(object)),
                        dimnames = list(rownames(setup$X),
                        names(fixef(object))))
            X[, colnames(setup$X)] <- setup$X
            setup$X <- X
        }
        newdata <- data.frame(X = setup$X[,1])
        newdata$X <- setup$X # only need X for prediction (not Y)
        if (1 %in% level && ncol(setup$random) != length(ranef(object))){
            ## expand to give col for every random effect
            Z <- matrix(0, nrow(setup$random), length(ranef(object)),
                        dimnames = list(rownames(setup$random),
                        colnames(object$random))) #ranef need names!!
            ## set to NA for contests with new players
            miss <- !colnames(setup$random) %in% colnames(Z)
            Z[, colnames(setup$random)[!miss]] <- setup$random[,!miss]
            if (any(miss)) {
                miss <- rowSums(setup$random[, miss, drop = FALSE] != 0) > 0
                Z[miss,] <- NA
            }
            setup$random <- Z
        }
        return(NextMethod(newrandom = setup$random))
    }
    else
        NextMethod()
}
