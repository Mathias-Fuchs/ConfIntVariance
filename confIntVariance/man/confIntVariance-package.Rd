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

##
## Example: throwing a dice
## 

                                        # True quantities that do not depend on n
trueMeanOfDice <- mean(1:6)




                                        # The true variance of the dice
                                        # This is the quantity that we want to estimate by embracing it with a confidence interval instead of just the point estimator contained in the function var
trueVarianceOfDice <- mean((1:6)^2) - trueMeanOfDice^2
trueFourthCentralMomentOfDice <- mean(((1:6)-trueMeanOfDice)^4)

                                        # this requires some scribbling with paper and pencil
                                        # (or a study of Hoeffding 1948)
trueVarianceOfSampleVarianceOfDice <- function(n) 
(trueFourthCentralMomentOfDice - trueVarianceOfDice^2 * (n-3)/(n-1))/n

##
## Simulation study: compute the coverage probability of
## the confidence interval by computing the probability
## that it contains the true value.
## We want that probability to be equal to the confidence level 0.95, not more and not less. (If it was higher, the test would be too conservative).
##


                                        # sample size
n <- 100
                                        # number of times we draw a sample and compute a confidence interval
N <- 1e4
trueValueCovered <- rep(NA, N)
for (i in 1:N) {
    if (i %% 1e3 == 0) print(i)
                                        # throw a dice n times
    x <- sample(6, n, replace=TRUE)
                                        # compute our confidence interval
    ci <- varwci(x)
                                        # We know that the true variance of the dice is 91/6 - 49/4 = 2.916666...
                                        # did the confidence interval contain the correct value?
    trueValueCovered[i] <- (trueVarianceOfDice > ci[1] && trueVarianceOfDice < ci[2])
}

                                        # Result of simulation study: should be close to 0.95
print(mean(trueValueCovered))
}
