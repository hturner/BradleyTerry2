## some awkward cases for predict
## (in response to bug reports from Arthur Spirling and Fonti Kar)

options(digits = 4) ## only applies to this file

## The final model in example(flatlizards)
library(BradleyTerry2)
data(flatlizards, package = "BradleyTerry2")
attach(flatlizards)
Whiting.model3 <- BTm(1, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                      head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = list(contests, predictors), trace = TRUE)


## new lizard with original lizards with NAs
newdata  <- list(contests = data.frame(winner = factor(c("lizard096", "lizard059"),
                                       levels = c("lizard048", "lizard052", "lizard096", "lizard059")),
                 loser = factor(c("lizard048", "lizard052"),
                 levels = c("lizard048", "lizard052", "lizard096", "lizard059"))),
                 predictors = rbind(flatlizards$predictors[c(27, 29, 55),-c(1,18) ],
                 c(NA, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 2000, 1, 0.1, 0.2, 0.5, -0.2)))

predict(Whiting.model3, level = 1, se.fit = TRUE, newdata = newdata)

## new lizard with NAs - can't predict, go by na.action
newdata  <- list(contests = data.frame(winner = factor(c("lizard048", "lizard059"),
                                       levels = c("lizard006", "lizard011", "lizard048", "lizard059")),
                 loser = factor(c("lizard006", "lizard011"),
                 levels = c("lizard006", "lizard011", "lizard048", "lizard059"))),
                 predictors = rbind(flatlizards$predictors[c(3, 6, 27),-c(1,18) ],
                 c(NA, 1.5, 1.5, -.2, 3, 1, -1, -1.5, -1.5, 250, 2000, 1, 0.1, 0.2, 0.5, -0.2)))

predict(Whiting.model3, level = 0:1, se.fit = TRUE, newdata = newdata, na.action = na.pass)

predict(Whiting.model3, level = 0:1, se.fit = TRUE, newdata = newdata, na.action = na.omit)

## newdata = original data
tmp <- predict(Whiting.model3)
tmp2 <- predict(Whiting.model3, newdata = list(contests, predictors))
identical(tmp, tmp2)

## new data with separate effects as in original

newdata  <- list(contests = data.frame(winner = factor(c("lizard096", "lizard099"),
                                       levels = c("lizard048", "lizard052", "lizard096", "lizard099")),
                 loser = factor(c("lizard048", "lizard052"),
                 levels = c("lizard048", "lizard052", "lizard096", "lizard099"))),
                 predictors = flatlizards$predictors[c(27, 29, 55, 56),-c(1,18) ])

predict(Whiting.model3, level = 1, se.fit = TRUE, newdata = newdata)#[31, 34]

predict(Whiting.model3, level = 1, se.fit = TRUE)$fit[c(31, 34)]
predict(Whiting.model3, level = 1, se.fit = TRUE)$se.fit[c(31, 34)]

## model in which some parameters are inestimable, e.g. contest-level predictor
## that is same for both players (interactions may be of interest in practice)

detach(flatlizards)
set.seed(1)
data(flatlizards)
flatlizards$contests$rainy <- sample(c(0, 1), nrow(flatlizards$contests), 
                                     replace = TRUE)
attach(flatlizards)
example.model <-  BTm(1, winner, loser, ~ rainy + throat.PC1[..] + throat.PC3[..] +
                          head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = list(contests, predictors), trace = TRUE)
example.model


head(predict(example.model, level = 0))
head(predict(example.model, level = 1))
lapply(predict(example.model, level = 0, se.fit = TRUE), head)
lapply(predict(example.model, level = 1, se.fit = TRUE), head)
# predict for unknown lizards - at level 0 only
newdata  <- list(contests = data.frame(
    rainy = c(0, 1),
    winner = factor(c("lizard100", "lizard101"),
                    levels = c("lizard100", "lizard101", "lizard102", "lizard103")),
    loser = factor(c("lizard103", "lizard102"),
                   levels = c("lizard100", "lizard101", "lizard102", "lizard103"))),
    predictors = as.data.frame(lapply(predictors, sample, 4)))
lapply(predict(example.model, level = 0, newdata = newdata, type = "response", 
               se.fit = TRUE), head)
# predict at level 1 for unknown lizards is NA
predict(example.model, level = 1, newdata = newdata, type = "response", se.fit = TRUE)
# predict at level 1 for known lizards
newdata$contests$winner <- factor(paste0("lizard0", c(10, 13)),
                                 levels = paste0("lizard0", 10:13))
newdata$contests$loser <- factor(c("lizard012", "lizard011"),
                                 levels = paste0("lizard0", 10:13))
newdata$predictors <- predictors[5:8,]
predict(example.model, level = 1, newdata = newdata, type = "response", se.fit = TRUE)
