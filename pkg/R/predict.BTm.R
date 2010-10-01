predict.BTm <- function (object, newdata = NULL, level = 0,
                         type = c("link", "response", "terms"), se.fit = FALSE,
                         dispersion = NULL, terms = NULL,
                         na.action = na.pass, ...) {
    type <- match.arg(type)
    if (!is.null(newdata)) {
        ## need to define X so will work with model terms
        setup <- match(c("player1", "player2", "formula", "id",
                        "separate.ability", "refcat", "weights",
                        "subset", "offset", "contrasts"), names(object$call), 0L)
        setup <- as.call(c(quote(BradleyTerry2:::BTm.setup),
                           as.list(object$call)[setup],
                           list(data = newdata)))
        setup <- eval(setup, environment(object$formula))
        nfix <- length(object$coefficients)
        if (ncol(setup$X) != nfix){
            ## will be due to separate abilities - else error by now
            X <- matrix(0, nrow(setup$X), nfix,
                        dimnames = list(rownames(setup$X),
                        names(object$coefficients)))
            X[, colnames(setup$X)] <- setup$X
            newdata <- data.frame(matrix(, nrow(X), 0))
            newdata$X <- X
        }
        nran <- length(attr(object$coefficients, "random"))
        if (1 %in% level && type != "terms" && ncol(setup$random) != nran){
            ## expand to give col for every random effect
            Z <- matrix(0, nrow(setup$random), nran,
                        dimnames = list(rownames(setup$random),
                        colnames(object$random))) #ranef need names!!
            ## set to NA for contests with new players
            miss <- !colnames(setup$random) %in% colnames(Z)
            Z[, colnames(setup$random)[!miss]] <- setup$random[,!miss]
            if (any(miss)) {
                miss <- rowSums(setup$random[, miss, drop = FALSE] != 0) > 0
                Z[miss,] <- NA
            }
            newrandom <- Z
            return(NextMethod(newrandom = newrandom))
        }
    }
    if (type == "terms") {
        object$x <- model.matrix(object)
        attr(object$x, "assign") <- object$assign
        id <- unique(object$assign)
        terms <- paste("X", id, sep = "")
        object$terms <- terms(reformulate(c(0, terms)))
        splitX <- function(X) {
            newdata <- data.frame(matrix(, nrow(X), 0))
            for (i in seq(id))
                newdata[terms[i]] <- X[,object$assign == id[i]]
            newdata
        }
        if (is.null(newdata)) newdata <- splitX(object$x)
        else newdata <- splitX(newdata$X)
        tmp <- NextMethod(newdata = newdata)
        #tmp$fit[tmp$se.fit == 0] <- NA
        tmp$se.fit[tmp$se.fit == 0] <- NA
        colnames(tmp$fit) <- colnames(tmp$se.fit) <-
            c("(separate)"[0 %in% id], object$term.labels)
        return(tmp)
    }
    else NextMethod()
}
