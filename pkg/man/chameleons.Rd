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
\usage{chameleons}
\format{
  A list containing three data frames: \code{chameleons$winner},
  \code{chameleons$loser} and \code{chameleons$predictors}.

  The \code{chameleons$winner} and \code{chameleons$loser} data frames
  each have 106 observations (one per contest) on the following 4 variables:
  \describe{
    \item{\code{ID}}{a factor with 35 levels \code{C01}, \code{C02},
      ... , \code{C43}, the identity of the winning (or losing) male in
      each contest}
    \item{\code{prev.wins.1}}{integer (values 0 or 1), did the
      winner/loser of this
      contest win in an immediately previous contest?}
    \item{\code{prev.wins.2}}{integer (values 0, 1 or 2), how many of his
      (maximum) previous 2 contests did each male win?}
    \item{\code{prev.wins.all}}{integer, how many previous contests has
      each male won?}
   }

   The \code{chameleons$predictors} data frame has 35 observations, one
   for each male involved in the contests, on the following 7 variables:
   \describe{
     \item{\code{ch.res}}{numeric, residuals of casque height regression
       on \code{SVL}, i.e. relative height of the bony part on the top
       of the chameleons' heads}
     \item{\code{jl.res}}{numeric, residuals of jaw length regression
       on \code{SVL}}
     \item{\code{tl.res}}{numeric, residuals of tail length regression
       on \code{SVL}}
     \item{\code{mass.res}}{numeric, residuals of body mass regression
       on \code{SVL} (body condition)}
     \item{\code{SVL}}{numeric, snout-vent length (body size)}
     \item{\code{prop.main}}{numeric, proportion (arcsin transformed)
       of area of the flank occupied by the main pink patch on the flank}
     \item{\code{prop.patch}}{numeric, proportion (arcsin transformed)
       of area of the flank occupied by the entire flank patch}
     }
}
\details{
The published paper mentions 107 contests, but only 106 contests are
included here.  Contest number 16 was
deleted from the data used to fit the models, because it involved a male
whose predictor-variables were incomplete (and it was the only contest
involving that lizard, so it is uninformative).
}
\source{
  The data were obtained by Dr Devi Stuart-Fox,
  \url{http://www.zoology.unimelb.edu.au/research/groups/animal/labs/stuart-fox/index.php},
   and they are reproduced here with her kind permission.

  These are the same data that were used in

  Stuart-Fox, D. M., Firth, D., Moussalli, A. and Whiting, M. J. (2006)
  Multiple signals in chameleon contests: designing and analysing animal
  contests as a tournament. \emph{Animal Behaviour} \bold{71},
  1263--1271.
}
\author{David Firth}
\examples{
##
## Reproduce Table 3 from page 1268 of the above paper:
##
summary(chameleon.model <- BTm(player1 = winner, player2 = loser,
  formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID), id = "ID",
  data = chameleons))
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
add1(chameleon.model, ~ . + jl.res[ID] + tl.res[ID] + mass.res[ID] +
  SVL[ID] + prop.patch[ID]) 
}
\keyword{datasets}
