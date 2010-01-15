library(BradleyTerry2)
data(citations, package = "BradleyTerry2")

## Convert frequencies to success/failure data
results <- countsToBinomial(xtabs(Freq ~ winner + loser,
    data = citations))
results
