# old test of BTabilities 
# - all parameters are estimable, not sure what this was about

## modelled by covariates where some parameters inestimable

summary(chameleon.model <- BTm(player1 = winner, player2 = loser,
                               formula = ~ prev.wins.2 + ch.res[ID] + 
                                   prop.main[ID] + (1|ID), 
                               id = "ID", data = chameleons))
head(BTabilities(chameleon.model))

# old test of grouped residuals 
# - there is no "separate" attribute here, has behaviour changed?

Whiting.model3 <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                          head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = flatlizards, trace = TRUE)

residuals(Whiting.model3, "grouped")
##  Note the "separate" attribute here, identifying two lizards with
##  missing values of at least one predictor variable