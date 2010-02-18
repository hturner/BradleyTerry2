\name{sound.fields}
\alias{sound.fields}
\docType{data}
\title{
Kousgaard (1984) Data on Pair Comparisons of Sound Fields
}
\description{
\emph{The results of a series of factorial subjective room acoustic
  experiments carried out at the Technical University of Denmark by A C Gade. }
}
\usage{data(sound.fields)}
\format{
A list containing two data frames, \code{sound.fields$comparisons},
and \code{sound.fields$design}.

The \code{sound.fields$comparisons} data frame has 84 observations on the following 8 variables:
 \describe{
   \item{\code{field1}}{a factor with levels \code{c("000", "001",
       "010", "011", "100", "101", "110", "111")}, the first sound
     field in a comparison}
   \item{\code{field2}}{a factor with the same levels as
     \code{field1}; the second sound field in a comparison}
   \item{\code{win1}}{integer, the number of times that
     \code{field1} was preferred to \code{field2}}
   \item{\code{tie}}{integer, the number of times that no
     preference was expressed when comparing \code{field1} and
     \code{field2}}
   \item{\code{win2}}{integer, the number of times that
     \code{field2} was preferred to \code{field1}}
   \item{\code{win1.adj}}{numeric, equal to \code{win1 + tie/2}}
   \item{\code{win2.adj}}{numeric, equal to \code{win2 + tie/2}}
   \item{\code{instrument}}{a factor with 3 levels,
     \code{c("cello", "flute", "violin")}}
 }

The \code{sound.fields$design} data frame has 8 observations (one for
each of the sound fields compared in the experiment)
on the following 3 variables:
\describe{
   \item{\code{a}}{a factor with levels \code{c("0",
       "1")}, the \emph{direct sound} factor (0 for \emph{obstructed
       sight line}, 1 for \emph{free sight line})}
   \item{\code{b}}{a factor with levels \code{c("0",
       "1")}, the \emph{reflection} factor (0 for \emph{-26dB},
     1 for \emph{-20dB})}
   \item{\code{c}}{a factor with levels \code{c("0",
       "1")}, the \emph{reverberation} factor (0 for \emph{-24dB},
     1 for \emph{-20dB})}
 }
 }
\details{
The variables \code{win1.adj} and \code{win2.adj} are provided in order
 to allow a simple way of handling ties (in which a tie counts as half a
 win and half a loss), which is slightly different numerically from the
 Davidson (1970) method that is used by Kousgaard (1984): see the
 examples.
}
\source{
Kousgaard, N. (1984)  Analysis of a Sound Field Experiment by a Model
for Paired Comparisons with Explanatory Variables.  \emph{Scandinavian
  Journal of Statistics} \bold{11}, 51--57.
 }
\references{
Davidson, R R (1970)  Extending the Bradley-Terry model to accommodate
 ties in paired comparison experiments.  \emph{Journal of the American
   Statistical Association} \bold{65}, 317--328.
}
\author{David Firth}
\examples{
data(sound.fields)
##
##  Fit the Bradley-Terry model to data for flutes, using the simple 'add 0.5'
##  method to handle ties:
##
\dontrun{
flutes.model <- BTm(cbind(win1.adj, win2.adj), field1, field2, ~ ..,
                    subset = (instrument == "flute"),
                    data = sound.fields)
}
}
\keyword{datasets}
