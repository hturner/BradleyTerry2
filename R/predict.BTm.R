#' Predict Method for Bradley-Terry Models
#' 
#' Obtain predictions and optionally standard errors of those predictions from
#' a fitted Bradley-Terry model.
#' 
#' If `newdata` is omitted the predictions are based on the data used for
#' the fit.  In that case how cases with missing values in the original fit are
#' treated is determined by the `na.action` argument of that fit.  If
#' `na.action = na.omit` omitted cases will not appear,
#' whereas if `na.action = na.exclude` they will appear (in predictions
#' and standard errors), with value `NA`.  See also `[napredict()]`.
#' 
#' @param object a fitted object of class `"BTm"`
#' @param newdata (optional) a single data frame of contest-level data or a 
#' list of data frames in which to look for variables with which to predict.  
#' If omitted, the fitted linear predictors are used.
#' @param level for models with random effects: an integer vector giving the
#' level(s) at which predictions are required. Level zero corresponds to
#' population-level predictions (fixed effects only), whilst level one
#' corresponds to the player-level predictions (full model) which are NA for
#' contests involving players not in the original data. By default, `level = 0`
#' for a fixed effects model, `1` otherwise.
#' @param type the type of prediction required.  The default `"link"`is to 
#' predict contest outcomes on the scale of the linear predictors, i.e. the 
#' log-odds (probabilities on logit scale) that the first player wins. The 
#' alternative `"response"` is on the scale of the binomial response variable, 
#' i.e. the predicted probability that the first player wins. The `"terms"` 
#' option returns a matrix giving the fitted values of each term in the model
#' formula on the linear predictor scale (fixed effects only). If 
#' `type = "ability"`, the fitted ability for each player in the contest is 
#' returned. When predicting abilities with new data, it is valid to specify 
#' only the first player (or only the second player) in a "contest".
#' @param se.fit logical switch indicating if standard errors are required.
#' @param dispersion a value for the dispersion, not used for models with
#' random effects. If omitted, that returned by `summary` applied to the
#' object is used, where applicable.
#' @param terms with `type ="terms"` by default all terms are returned.  A
#' character vector specifies which terms are to be returned.
#' @param na.action function determining what should be done with missing
#' values in `newdata`.  The default is to predict `NA`.
#' @param \dots further arguments passed to or from other methods.
#' @return If `se.fit = FALSE`, a vector or matrix of predictions.  If
#' `se = TRUE`, a list with components \item{fit }{Predictions}
#' \item{se.fit }{Estimated standard errors}
#' @author Heather Turner
#' @seealso [predict.glm()], [predict.glmmPQL()]
#' @keywords models
#' @examples
#' 
#' ## The final model in example(flatlizards)
#' result <- rep(1, nrow(flatlizards$contests))
#' Whiting.model3 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
#'                       head.length[..] + SVL[..] + (1|..),
#'                       family = binomial(link = "probit"),
#'                       data = flatlizards, trace = TRUE)
#' 
#' ## `new' data for contests between four of the original lizards
#' ## factor levels must correspond to original levels, but unused levels
#' ## can be dropped - levels must match rows of predictors
#' newdata  <- list(contests = data.frame(
#'                  winner = factor(c("lizard048", "lizard060"),
#'                  levels = c("lizard006", "lizard011", 
#'                             "lizard048", "lizard060")),
#'                  loser = factor(c("lizard006", "lizard011"),
#'                  levels = c("lizard006", "lizard011", 
#'                             "lizard048", "lizard060"))
#'                  ),
#'                  predictors = flatlizards$predictors[c(3, 6, 27, 33), ])
#' 
#' predict(Whiting.model3, level = 1, newdata = newdata)
#' 
#' ## same as
#' predict(Whiting.model3, level = 1)[1:2]
#' 
#' ## introducing a new lizard
#' newpred <- rbind(flatlizards$predictors[c(3, 6, 27),
#'                      c("throat.PC1","throat.PC3", "SVL", "head.length")],
#'                  c(-5, 1.5, 1, 0.1))
#' rownames(newpred)[4] <- "lizard059"
#' 
#' newdata  <- list(contests = data.frame(
#'                  winner = factor(c("lizard048", "lizard059"),
#'                  levels = c("lizard006", "lizard011", 
#'                             "lizard048", "lizard059")),
#'                  loser = factor(c("lizard006", "lizard011"),
#'                  levels = c("lizard006", "lizard011", 
#'                             "lizard048", "lizard059"))
#'                  ),
#'                  predictors = newpred)
#' 
#' ## can only predict at population level for contest with new lizard
#' predict(Whiting.model3, level = 0:1, se.fit = TRUE, newdata = newdata)
#' 
#' ## predicting at specific levels of covariates
#' 
#' ## consider a model from example(CEMS)
#' table6.model <-  BTm(outcome = cbind(win1.adj, win2.adj),
#'                      player1 = school1, player2 = school2,
#'                      formula = ~ .. +
#'                          WOR[student] * Paris[..] +
#'                          WOR[student] * Milano[..] +
#'                          WOR[student] * Barcelona[..] +
#'                          DEG[student] * St.Gallen[..] +
#'                          STUD[student] * Paris[..] +
#'                          STUD[student] * St.Gallen[..] +
#'                          ENG[student] * St.Gallen[..] +
#'                          FRA[student] * London[..] +
#'                          FRA[student] * Paris[..] +
#'                          SPA[student] * Barcelona[..] +
#'                          ITA[student] * London[..] +
#'                          ITA[student] * Milano[..] +
#'                          SEX[student] * Milano[..],
#'                      refcat = "Stockholm",
#'                      data = CEMS)
#'                      
#' ## estimate abilities for a combination not seen in the original data
#' 
#' ## same schools
#' schools <- levels(CEMS$preferences$school1)
#' ## new student data
#' students <- data.frame(STUD = "other", ENG = "good", FRA = "good", 
#'                        SPA = "good", ITA = "good", WOR = "yes", DEG = "no",
#'                        SEX = "female", stringsAsFactors = FALSE)
#' ## set levels to be the same as original data    
#' for (i in seq_len(ncol(students))){
#'     students[,i] <- factor(students[,i], levels(CEMS$students[,i]))
#' }
#' newdata <- list(preferences = 
#'     data.frame(student = factor(500), # new id matching with `students[1,]`
#'                school1 = factor("London", levels = schools),
#'                school2 = factor("Paris", levels = schools)),
#'     students = students,
#'     schools = CEMS$schools)
#' 
#' ## warning can be ignored as model specification was over-parameterized
#' predict(table6.model, newdata = newdata)
#' 
#' ## if treatment contrasts are use (i.e. one player is set as the reference
#' ## category), then predicting the outcome of contests against the reference
#' ## is equivalent to estimating abilities with specific covariate values
#' 
#' ## add student with all values at reference levels 
#' students <- rbind(students,
#'     data.frame(STUD = "other", ENG = "good", FRA = "good", 
#'                SPA = "good", ITA = "good", WOR = "no", DEG = "no",
#'                SEX = "female", stringsAsFactors = FALSE))
#' ## set levels to be the same as original data    
#' for (i in seq_len(ncol(students))){
#'     students[,i] <- factor(students[,i], levels(CEMS$students[,i]))
#' }
#' newdata <- list(preferences = 
#'     data.frame(student = factor(rep(c(500, 502), each = 6)), 
#'                school1 = factor(schools, levels = schools),
#'                school2 = factor("Stockholm", levels = schools)),
#'     students = students,
#'     schools = CEMS$schools)
#'     
#' predict(table6.model, newdata = newdata, se.fit = TRUE)
#' 
#' ## the second set of predictions (elements 7-12) are equivalent to the output 
#' ## of BTabilities; the first set are adjust for `WOR` being equal to "yes"
#' BTabilities(table6.model)
#' 
#' @importFrom stats model.matrix na.pass reformulate
#' @export
predict.BTm <- function (object, 
                         newdata = NULL, 
                         level = ifelse(is.null(object$random), 0, 1),
                         type = c("link", "response", "terms", "ability"), 
                         se.fit = FALSE,
                         dispersion = NULL,
                         terms = NULL,
                         na.action = na.pass, ...) {
    type <- match.arg(type)
    # type = "ability" ends up equivalent to type = "link"
    if (type == "ability"){
        if (is.null(newdata)){
            # TODO::define newdata based on original model, 
            # will need to set type <- "link"
            stop("not yet implemented")
        }
    }
    if (!is.null(newdata)) {
        # need to define X so will work with model terms
        ## use player1 (and player2) elements from newdata
        player1 <- newdata$player1
        player2 <- newdata$player2
        if (is.null(player1) & is.null(player2)){
            getPlayer<- function(data, name){
                if (is.data.frame(data)) return(data[[name]])
                for (i in seq_along(data)){
                    x <- data[[i]][[name]]
                    if (!is.null(x)) return(x)
                }
                x
            }
            # use names from original call to identify player1 and player2
            player1 <- getPlayer(newdata, deparse(object$call$player1))
            if (!is.null(player1)){
                player2 <- getPlayer(newdata, deparse(object$call$player2))
            } else {
                # use id to identify player 1, can only be one player
                player1 <- getPlayer(newdata, object$id)
            }
        }
        if (is.null(player1)) stop ("Cannot identify player 1 from `newdata`.")
        if (is.null(player2) & type != "ability")
            stop("`newdata` must specify both players for `type != \"ability\"`")
        if (type == "ability"){
            type <- "link"
            ## predict player 1
            setup <- getXZ(object, player1, NULL, newdata, level, type)
            newdata <- setup$newdata
            pred1 <- NextMethod(newrandom = setup$newrandom)
            if (!is.null(player2)){
                setup <- getXZ(object, NULL, player2, newdata, level, type)
                newdata <- setup$newdata
                pred2 <- NextMethod(newrandom = setup$newrandom)
                return(list(fit = list(ability = pred1$fit,
                                       ability2 = pred2$fit),
                            se.fit = list(ability = pred1$se.fit,
                                          ability2 = pred2$se.fit)))
            } else {
                return(pred1)
            }
        } else {
            setup <- getXZ(object, player1, NULL, newdata, level, type)
            newdata <- setup$newdata
            newrandom <- setup$newrandom
        }
    }
    if (type == "terms") {
        object$x <- model.matrix(object)
        attr(object$x, "assign") <- object$assign
        id <- unique(object$assign)
        terms <- paste("X", id, sep = "")
        object$terms <- terms(reformulate(c(0, terms)))
        splitX <- function(X) {
            newdata <- data.frame(matrix(nrow = nrow(X), ncol = 0))
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
    NextMethod(newrandom = setup$newrandom)
}

getXZ <- function(object, player1, player2, newdata, level, type){
    setup <- do.call(BTm.setup,
                     list(player1 = player1,
                          player2 = player2,
                          formula = formula(object),
                          id = object$id,
                          separate.ability = object$separate.ability,
                          refcat = object$refcat,
                          data = newdata, 
                          weights = object$weights,
                          offset = NULL, # only makes sense to add offset to full predictor
                          contrasts = object$contrasts),
                     envir = parent.frame())
    newdata <- data.frame(matrix(nrow = nrow(setup$X), ncol = 0))
    # define fixed effects
    nfix <- length(object$coefficients)
    keep <- match(names(object$coefficients), colnames(setup$X),
                  nomatch = 0)
    if (0 %in% keep){
        ## new players with missing data - set to NA
        missing <- rowSums(setup$X[,-keep, drop = FALSE]) != 0
        setup$X <- setup$X[, keep]
        setup$X[missing,] <- NA
    }
    if (ncol(setup$X) != nfix) {
        ## newdata does not include original players with missing data
        X <- matrix(0, nrow(setup$X), nfix,
                    dimnames = list(rownames(setup$X),
                                    names(object$coefficients)))
        X[, colnames(setup$X)] <- setup$X
        newdata$X <- X
    } else newdata$X <- setup$X
    # define random effects if necessary
    nran <- length(attr(object$coefficients, "random"))
    if (1 %in% level && !is.null(object$random) && type != "terms"){
        if (ncol(setup$random) != nran) {
            ## expand to give col for every random effect
            Z <- matrix(0, nrow(setup$random), nran,
                        dimnames = list(rownames(setup$random),
                                        colnames(object$random))) #ranef need names!!
            ## set to NA for contests with new players 
            ## (with predictors present)
            miss <- !colnames(setup$random) %in% colnames(Z)
            Z[, colnames(setup$random)[!miss]] <- setup$random[,!miss]
            if (any(miss)) {
                miss <- rowSums(setup$random[, miss, drop = FALSE] != 0) > 0
                Z[miss,] <- NA
            }
            return(list(newdata = newdata, newrandom = Z))
        } else return(list(newdata = newdata, newrandom = setup$random))
    }

    return(list(newdata = newdata, newrandom = NULL))
}
    