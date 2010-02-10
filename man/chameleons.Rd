\name{chameleons}
\alias{chameleons}
\docType{data}
\title{
Male Cape Dwarf Chameleons: Measured Traits and Contest Outcomes
}
\description{
Data as used in the study by Stuart-Fox et al. (2006).  Physical
measurements made on 35 male Cape dwarf chameleons, and the results of
106 inter-male contests.
}
\usage{data(chameleons)}
\format{
  The format is:
List of 3 data frames:
 $winner    :'data.frame':	106 obs. (contests), 4 variables:
  ..$ID           : Factor w/ 35 levels
  "C01","C02","C03",...,"C41", the identity of the winning lizard in
  each contest
  ..$prev.wins.1  : integer (values 0 or 1), did the winner of this
  contest win in his previous contest (if there was one)?
  ..$prev.wins.2  : integer (values 0, 1 or 2), how many of his (maximum)
  previous 2 contests did this winner win?
  ..$prev.wins.all: integer, how many previous contests has this
  male won?
 $loser     :'data.frame':	106 obs. of  4 variables:   same
 structure as $winner, but variables now corresponding to the loser of
 the contest
 $ predictors:'data.frame':	35 obs. of  7 variables:
  ..$ch.res    : numeric, residuals of casque height regression on SVL, i.e. relative height of the knobbly bony bit on the top of the chameleons' heads
  ..$jl.res    : numeric, residuals of jaw length regression on SVL
  ..$tl.res    : numeric, residuals of tail length regression on on SVL
  ..$mass.res  : numeric, residuals of body mass regression on SVL (body condition)
  ..$ SVL       : numeric, snout-vent length (body size)
  ..$ prop.main : numeric, proportion (arcsin transformed) of area of the flank occupied by the main pink patch on the flank
  ..$ prop.patch: numeric, proportion (arcsin transformed) of area of the flank occupied by the entire flank patch
}
\details{
The published paper mentions 107 contests, but only 106 contests are
included here.  Contest number 16 was
deleted from the data used to fit the models, because it involved a male
whose predictor-variables were incomplete (and it was the only contest
involving that lizard, so it is uninformative).
}
\source{
  Dr Devi Stuart-Fox.  These are the same data that were used in

Stuart-Fox, D M, Firth, D, Moussalli, A and Whiting, M J (2006) Multiple signals in chameleon contests: designing and analysing animal contests as a tournament. \emph{Animal Behaviour} \bold{71}, 1263--1271.
}
\examples{
data(chameleons)
##
## Reproduce Table 3 from page 1268 of the above paper:
##
summary(chameleon.model <- BTm(rep(1, 106), winner, loser, formula = ~
prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID), id = "ID", data =
chameleons))
##
## Note that, although a per-chameleon random effect is specified as in the
## above [the term "+ (1|ID)"], the estimated variance for that random
## effect turns out to be zero in this case.  The "prior experience"
## effect ["+ prev.wins.2"] in this analysis has explained most of the
## variation, leaving little for the ID-specific predictors to do.
## Despite that, two of the ID-specific predictors do emerge as
## significant.
##
## Test whether any of the other ID-specific predictors has an effect:
##
add1(chameleon.model, ~ . + jl.res[ID] + tl.res[ID] + mass.res[ID] + SVL[ID]
+ prop.patch[ID]) 
}
\keyword{datasets}
