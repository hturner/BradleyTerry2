context("methods [add1, drop1]")

tol <- 1e-6

## some awkward cases for predict
## (in response to bug reports from Arthur Spirling and Fonti Kar)

## Case 1: The final model in example(flatlizards)

Whiting.model3 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                      head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = flatlizards)

## add new lizard (54, 59)
lev <- c(levels(flatlizards$contests$winner), "lizard054", "lizard059")
## add features for new lizards (excluding factor variables for convenience)
## 59 has missing values for some model predictors
features <- rbind(flatlizards$predictors[, -c(1,18)],
                  c(1.5, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 
                    2000, 1, 0.1, 0.2, 0.5, -0.2),
                  c(NA, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 
                    2000, 1, 0.1, 0.2, 0.5, -0.2))

## alternatively create new data just for lizards of interest: lev must match
lev2 <- c("lizard048", "lizard052", "lizard099", "lizard054", "lizard059")
features2  <- rbind(flatlizards$predictors[c(27, 29, 56),-c(1,18) ],
                    c(1.5, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 
                      2000, 1, 0.1, 0.2, 0.5, -0.2),
                    c(NA, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 
                      2000, 1, 0.1, 0.2, 0.5, -0.2))

test_that("predict on original data same as original fit", {
    tmp <- predict(Whiting.model3)
    tmp2 <- predict(Whiting.model3, newdata = flatlizards)
    expect_identical(tmp, tmp2)
})

test_that("predict works at level 0 only with new lizard", {
    newdata  <- list(contests = 
                         data.frame(winner = factor("lizard054", levels = lev),
                                    loser = factor("lizard048", levels = lev)),
                     predictors = features)
    pred0 <- predict(Whiting.model3, level = 0, se.fit = TRUE, 
                     newdata = newdata)
    expect_known_value(pred0,
                       file = test_path("outputs/flatlizards-pred0-new.rds"),
                       tol = tol)
    pred1 <- predict(Whiting.model3, level = 1, se.fit = TRUE, 
                     newdata = newdata)
    expect_true(all(is.na(pred1)))
    # use alternative newdata
    newdata  <- list(contests = 
                         data.frame(winner = factor("lizard054", levels = lev2),
                                    loser = factor("lizard048", levels = lev2)),
                     predictors = features2)
    pred0b <- predict(Whiting.model3, level = 0, se.fit = TRUE, 
                      newdata = newdata)
    pred1b <- predict(Whiting.model3, level = 1, se.fit = TRUE, 
                      newdata = newdata)
    expect_identical(pred0, pred0b)
    expect_identical(pred1, pred1b)
})

test_that("predict works for original lizard with NA predictors", {
    newdata  <- list(contests = 
                         data.frame(winner = factor("lizard099", levels = lev),
                                    loser = factor("lizard052", levels = lev)),
                     predictors = features)
    # predict based on "new" data
    pred0a <- predict(Whiting.model3, level = 0, se.fit = TRUE, 
                      newdata = newdata)
    pred1a <- predict(Whiting.model3, level = 1, se.fit = TRUE, 
                      newdata = newdata)
    # should be same as original fit
    pred0b <- predict(Whiting.model3, level = 0, se.fit = TRUE)
    pred1b <- predict(Whiting.model3, level = 1, se.fit = TRUE)
    expect_equal(pred0a$fit, pred0b$fit[34])
    expect_equal(pred0a$se.fit, pred0b$se.fit[34])
    expect_equal(pred1a$fit, pred1b$fit[34])
    expect_equal(pred1a$se.fit, pred1b$se.fit[34])
    # use alternative newdata
    newdata  <- list(contests = 
                         data.frame(winner = factor("lizard099", levels = lev2),
                                    loser = factor("lizard052", levels = lev2)),
                     predictors = features2)
    pred0b <- predict(Whiting.model3, level = 0, se.fit = TRUE, 
                      newdata = newdata)
    pred1b <- predict(Whiting.model3, level = 1, se.fit = TRUE, 
                      newdata = newdata)
    expect_identical(pred0a, pred0b)
    expect_identical(pred1a, pred1b)
})

test_that("predict respects na.action for new lizard with NA", {
    newdata  <- list(contests = 
                         data.frame(winner = factor(c("lizard099", "lizard059"),
                                                    levels = lev),
                                    loser = factor(c("lizard052", "lizard048"),
                                                   levels = lev)),
                     predictors = features)
    # keep NA where prediction not possible (due to NAs in predictors)
    pred_na_pass <- predict(Whiting.model3, level = 0:1, se.fit = TRUE, 
                            newdata = newdata, na.action = na.pass)
    # predictions for contest 1 should be as original fit, contest 2 NA
    pred <- predict(Whiting.model3, level = 0:1, se.fit = TRUE)
    expect_equal(pred_na_pass$population$fit[1], pred$population$fit[34])
    expect_equal(pred_na_pass$population$se.fit[1], pred$population$se.fit[34])
    expect_equal(pred_na_pass$individual$fit[1], pred$individual$fit[34])
    expect_equal(pred_na_pass$individual$se.fit[1], pred$individual$se.fit[34])
    expect_true(all(is.na(c(pred_na_pass$population$fit[2],
                            pred_na_pass$population$se.fit[2],
                            pred_na_pass$individual$fit[2],
                            pred_na_pass$individual$se.fit[2]))))
    # remove NA with na.omit
    pred_na_omit <- predict(Whiting.model3, level = 0:1, se.fit = TRUE, 
                            newdata = newdata, na.action = na.omit)
    expect_equal(pred_na_pass$population$fit[1], pred_na_omit$population$fit[1])
    expect_equal(pred_na_pass$population$se.fit[1], 
                 pred_na_omit$population$se.fit[1])
    expect_equal(pred_na_pass$individual$fit[1], pred_na_omit$individual$fit[1])
    expect_equal(pred_na_pass$individual$se.fit[1], 
                 pred_na_omit$individual$se.fit[1])
    # use alternative newdata
    newdata  <- list(contests = 
                         data.frame(winner = factor(c("lizard099", "lizard059"),
                                                    levels = lev2),
                                    loser = factor(c("lizard052", "lizard048"),
                                                   levels = lev2)),
                     predictors = features2)
    pred_na_pass2 <- predict(Whiting.model3, level = 0:1, se.fit = TRUE, 
                             newdata = newdata, na.action = na.pass)
    pred_na_omit2 <- predict(Whiting.model3, level = 0:1, se.fit = TRUE, 
                             newdata = newdata, na.action = na.omit)
    expect_identical(pred_na_pass, pred_na_pass2)
    expect_identical(pred_na_omit, pred_na_omit2)
})

## Case 2: model in which some parameters are inestimable, e.g. contest-level 
## predictor that is same for both players (interactions may be of interest in 
## practice)

### set seed for consistency with historical results 
### (when sampling predictor values for new hypothetical lizards)
suppressWarnings(RNGversion("2.10")) 
set.seed(1)
flatlizards$contests$rainy <- sample(c(0, 1), nrow(flatlizards$contests), 
                                     replace = TRUE)
### "rainy" main effect is inestimable
example.model <-  BTm(1, winner, loser, ~ rainy + 
                          throat.PC1[..] + throat.PC3[..] +
                          head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = flatlizards)

## create data for 4 new lizards (sample data from of old lizards)
lev <- c("lizard100", "lizard101", "lizard102", "lizard103")
newdata  <- list(contests = data.frame(
    rainy = c(0, 1),
    winner = factor(c("lizard100", "lizard101"),
                    levels = lev),
    loser = factor(c("lizard103", "lizard102"),
                   levels = lev)),
    predictors = as.data.frame(lapply(flatlizards$predictors, sample, 4)))

# or new data for 4 old lizards
id <- 5:8
lev <- paste0("lizard0", 10:13)
newcontests  <- list(contests = data.frame(
    rainy = c(0, 1),
    winner = factor(c("lizard010", "lizard013"), levels = lev),
    loser = factor(c("lizard012", "lizard011"), levels = lev)),
    predictors = flatlizards$predictors[id,])

test_that("predict as expected for model with inestimable par", {
    ## no se
    pred0a <- predict(example.model, level = 0)
    pred1a <- predict(example.model, level = 1)
    ## with se
    pred0b <- predict(example.model, level = 0, se.fit = TRUE)
    pred1b <- predict(example.model, level = 1, se.fit = TRUE)
    ## predictions (fitted values) are the same
    expect_equal(pred0a, pred0b$fit)
    expect_equal(pred1a, pred1b$fit)
})

test_that("predict works for unknown lizards at level 0 only", {
    pred0 <- predict(example.model, level = 0, newdata = newdata,
                     type = "response", se.fit = TRUE)
    expect_known_value(pred0,
                       file = test_path("outputs/flatlizards-pred0-rainy.rds"),
                       tol = tol)
    pred1 <- predict(example.model, level = 1, newdata = newdata,
                     type = "response", se.fit = TRUE)
    expect_true(all(is.na(unlist(pred1))))
})

test_that("predict works for known lizards at level 1", {
    pred1 <- predict(example.model, level = 1, newdata = newcontests,
                     type = "response", se.fit = TRUE)
    expect_known_value(pred1,
                       file = test_path("outputs/flatlizards-pred1-rainy.rds"),
                       tol = tol)
})