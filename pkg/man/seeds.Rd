\name{seeds}
\alias{seeds}
\docType{data}
\title{
Seed Germination Data from Crowder (1978)
}
\description{
Data from Crowder(1978) giving the proportion of seeds germinated for
21 plates that were arranged according to a 2x2 factorial layout by seed
variety and type of root extract.
}
\usage{data(seeds)}
\format{
  A data frame with 21 observations on the following 4 variables.
  \describe{
    \item{\code{r}}{the number of germinated seeds.}
    \item{\code{n}}{the total number of seeds.}
    \item{\code{seed}}{the seed variety.}
    \item{\code{extract}}{the type of root extract.}
  }
}
\source{
Crowder, M. (1978) Beta-Binomial ANOVA for proportions. \emph{Applied Statistics}, \bold{27}, 34--37. 
}
\references{
  Breslow, N. E. and Clayton, D. G. (1993) Approximate inference
  in Generalized Linear Mixed Models. \emph{Journal of the American
    Statistical Association}, \bold{88}(421), 9--25.
}
\seealso{\code{\link{glmmPQL}}}
\examples{
data(seeds)
attach(seeds)

summary(glmmPQL(cbind(r, n - r) ~ seed + extract,
        random = diag(length(r)),
        family = binomial))
}
\keyword{datasets}
