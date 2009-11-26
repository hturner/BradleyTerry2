\name{chameleons}
\alias{chameleons}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(chameleons)}
\format{
  This dataset is a list containing two data frames: \code{chameleons$contests}
  and \code{chameleons$predictors}.

  The \code{chameleons$contests} data frame has 107 observations on the
  following variables:
  \describe{
    \item{\code{winner}}{ a factor with 36 levels \code{"C1"}
      ... \code{"C41"}}
    \item{\code{loser}}{ a factor with the same 36 levels \code{"C1"}
      ... \code{"C41"}}
    \item{\code{expA}}{ a matrix of 2 variables, \code{"pwins"} and
	\code{"ploss"}}
    \item{\code{exp1}}{ a matrix of 2 variables, \code{"pwins"} and
	\code{"ploss"}} 
    \item{\code{exp2}}{ a matrix of 2 variables, \code{"pwins"} and
	\code{"ploss"}} 
    \item{\code{expW}}{ a matrix of 2 variables, \code{"pwins"} and
	\code{"ploss"}} 
  }

  The \code{chameleons$predictors} data frame has 36 observations (one
  for each of the 36 chameleons) on the following 7 variables:  
  \describe{
    \item{ch.res}{numeric, }
    \item{jl.res}{numeric, }
    \item{tl.res}{numeric, }
    \item{mass.res}{numeric, }
    \item{SVL}{numeric, }
    \item{prop.main}{numeric, }
    \item{prop.patch}{numeric, }
  }
}
\details{
  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(chameleons)
## maybe str(chameleons) ; plot(chameleons) ...
}
\keyword{datasets}
