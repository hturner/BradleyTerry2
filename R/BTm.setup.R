#' @importFrom stats reformulate
BTm.setup <- function(player1, player2 = NULL, outcome = 1, formula = NULL,
                      id = "..", separate.ability = NULL, refcat = NULL,
                      data = NULL, weights = NULL, subset = NULL,
                      offset = NULL, contrasts = NULL, ...){
    if (!is.data.frame(data)){
        keep <- names(data) %in% c(deparse(substitute(player1)),
                                   deparse(substitute(player2)))
        if (!length(keep)) keep <- FALSE
        ## save row names for checking against index variables (in Diff)
        data <- lapply(data, as.data.frame)
        nm <- lapply(data, rownames)
        data <- c(data[keep], unlist(unname(data[!keep]), recursive = FALSE))
        if (any(dup <- duplicated(names(data))))
            warning("'data' argument specifies duplicate variable names: ",
                    paste(names(data)[dup], collapse = " "))
    }
    ## (will take first occurence of replicated names)
    withIfNecessary <- function(x, formula, data = NULL, as.data.frame = TRUE) {
        if (as.data.frame)
            expr <- substitute(data.frame(x), list(x = x))
        else expr <- x
        eval(expr, data, enclos = environment(formula))
    }
    player1 <- withIfNecessary(substitute(player1), formula, data)
    if (ncol(player1) == 1) colnames(player1) <- id
    if (ncol(player1) == 0) player1 <- NULL
    player2 <- withIfNecessary(substitute(player2), formula, data)
    if (ncol(player2) == 1) colnames(player2) <- id
    if (ncol(player2) == 0) player2 <- NULL
    Y <- withIfNecessary(substitute(outcome), formula, 
                         c(player1, player2, data), as.data.frame = FALSE)
    weights <- withIfNecessary(substitute(weights), formula, data, FALSE)
    subset1 <- withIfNecessary(substitute(subset), formula, 
                               c(player1 = list(player1),
                                 player2 = list(player2), player1, data), FALSE)
    subset2 <- withIfNecessary(substitute(subset), formula, 
                               c(player1 = list(player1),
                                 player2 = list(player2), player2, data), FALSE)
    if (is.logical(subset1)) subset <- subset1 | subset2
    else subset <- c(subset1, subset2)
    diffModel <- Diff(player1, player2, formula, id, data, separate.ability,
                      refcat, contrasts, nm)
    offset <- withIfNecessary(substitute(offset), formula, data, FALSE) #contest level
    if (!is.null(offset)) {
        if (is.null(diffModel$offset))  diffModel$offset <- offset
        else diffModel$offset <- diffModel$offset + offset
    }
    res <- c(diffModel, list(data = data, player1 = player1, player2 = player2,
                             Y = Y, weights = weights, subset = subset, 
                             formula = formula))
}
