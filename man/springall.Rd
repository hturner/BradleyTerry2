\name{springall}
\alias{springall}
\docType{data}
\title{
Experiment to Assess the Effect of Gel and Flavour Concentration on Subjective Evaluation of Flavour Strength
}
\description{
Data from Section 7 of the paper by Springall (1973) on Bradley-Terry response surface modelling.
}
\usage{data(springall)}
\format{
A list containing two data frames, \code{springall$contests} and \code{springall$predictors}.

The \code{springall$contests} data frame has 36 observations (one for each possible pairwise comparison of the 9 treatments) on the following 7 variables:
 \describe{
	\item{\code{row}}{a factor with levels \code{1:9}, the row number in Springall's dataset}
        \item{\code{col}}{a factor with levels \code{1:9}, the column number in Springall's dataset}
        \item{\code{win}}{integer, the number of wins for column treatment over row treatment}
        \item{\code{loss}}{integer, the number of wins for row treatment over column treatment}
        \item{\code{tie}}{integer, the number of ties between row and column treatments}
        \item{\code{win.adj}}{numeric, equal to \code{win + tie/2}}
        \item{\code{loss.adj}}{numeric, equal to \code{loss + tie/2}}
	}

The \code{predictors} data frame has 9 observations (one for each treatment) on the following 5 variables:
 \describe{
	\item{\code{flav}}{numeric, the flavour concentration}
 	\item{\code{gel}}{numeric, the gel concentration}
  	\item{\code{flav.2}}{numeric, equal to \code{flav ^ 2}}
	\item{\code{gel.2}}{numeric, equal to \code{gel ^ 2}}
        \item{\code{flav.gel}}{numeric, equal to \code{flav * gel}}
}
}
\details{
The variables \code{win.adj} and \code{loss.adj} are provided in order to allow a simple way of handling ties (in which a tie counts as half a win and half a loss), which is slightly different numerically from the Rao and Kupper (1967) model that Springall (1973) uses.
}
\source{
Springall, A (1973)  Response surface fitting using a generalization of the Bradley-Terry paired comparison method.  \emph{Applied Statistics} \bold{22}, 59--68.
}
\references{
Rao, P V and Kupper, L L (1967)  Ties in paired-comparison experiments: a generalization of the Bradley-Terry model.  \emph{Journal of the American Statistical Association}, \bold{63}, 194--204.
}
\examples{
data(springall)
##
## Fit the same response-surface model as in section 7 of Springall (1973):
##
summary(springall.model <- BTm(cbind(win.adj, loss.adj), springall$contests$col, springall$contests$row, ~ flav[..] + gel[..] + flav.2[..] + gel.2[..] + flav.gel[..] + (1 | ..), data = springall) )
##
## Differences from Springall's fit are minor, arising from the 
## different treatment of ties.
##
## Springall's model does not include the random effect.  In this case,
## however, that makes no difference: the random-effect variance is 
## estimated as zero.
##
}
\keyword{datasets}
