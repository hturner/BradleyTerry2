library(BradleyTerry2)

## refcat is not the first level

##  Convert frequencies to success/failure data
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")

##  First fit the "standard" Bradley-Terry model
citeModel <- BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
BTabilities(citeModel)

##  Now the same thing with a different "reference" journal
citeModel2 <- update(citeModel, refcat = "JASA")
BTabilities(citeModel2)

## modelled by covariates where some parameters inestimable

summary(chameleon.model <- BTm(player1 = winner, player2 = loser,
   formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID), id = "ID",
   data = chameleons))
head(BTabilities(chameleon.model))

## sum to zero contrasts

mod3 <- BTm(cbind(win1, win2), journal1,
            journal2, ~ journal, id = "journal", x = FALSE, 
            contrasts = list(journal = "contr.sum"), data = citations.sf)

citations.sf$journal1 <- C(citations.sf$journal1, "contr.sum")
citations.sf$journal2 <- C(citations.sf$journal2, "contr.sum") 
mod3b <-
    BTm(cbind(win1, win2), journal1, journal2, ~ journal, id = "journal", 
        x = FALSE, data = citations.sf)

BTabilities(mod3) 
BTabilities(mod3b)
