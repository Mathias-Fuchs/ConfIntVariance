\name{confIntVariance-package}
\alias{confIntVariance-package}
\alias{confIntVariance}
\docType{package}
\title{
\packageTitle{confIntVariance}
}
\description{
\packageDescription{confIntVariance}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{confIntVariance}
\packageIndices{confIntVariance}
A package providing one function varwci which is short for
"variance with confidence interval."
}
\author{
\packageAuthor{Mathias Fuchs}
Maintainer: \packageMaintainer{Mathias Fuchs}
}
\references{
  www.mathiasfuchs.de/b3.html
}
\keyword{variance, confidence interval}
\examples{
s <- sample(100, 6)
                                        # The confidence interval for the true variance of the dice.
                                        # We know that the true variance of the dice is 91/6 - 49/4 = 2.916666...
                                        # So, the confidence interval will contain that value in pretty exactly 95% of all cases.
                                        # You can check that.
varwci(s) 
}
