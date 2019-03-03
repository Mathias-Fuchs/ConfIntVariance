
productsOfDisjointTuples <- function(s) {
    n <- length(s)
    N <- matrix(0, ncol = n, nrow=min(n, 4))
    N[1, 1] <- s[1]
    for (i in seq(2, n)) {
        N[1, i] <- s[i] + N[1, i-1]
        for (j in seq(2, min(n, 4)))
            N[j, i] <- s[i] * N[j - 1, i-1] + N[j, i-1]
    }    
    N[, n]
}

varianceOfSampleVariance <- function(x) {
    v <- var(x)
    p1 <- productsOfDisjointTuples(x)
    p2 <- productsOfDisjointTuples(x^2)
    var(x)^2 - p2[2] / choose(n, 2) + 4/n/(n-1)/(n-2)*(p1[3]*sum(x) - 4 * p1[4]) - p1[4]/choose(n, 4)
}

                                        # the confidence interval for the variance
varwci <- function(x, conf.level=0.95) {
    n <- length(x)
    t <- qt((1+conf.level)/2, df=n-1)
    v <- var(x)
    varsv <- varianceOfSampleVariance(x)
    if (varsv < 0) {
        warning("Sample size too small for estimation of the variance of the sample variance")
        return(c(NA, NA))
    }
    c(v-t*sqrt(varsv), v + t*sqrt(varsv))
}

##
## Example: throwing a dice
## 

                                        # True quantities that do not depend on n
trueMeanOfDice <- mean(1:6)
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
                                        # did the confidence interval contain the correct value?
    trueValueCovered[i] <- (trueVarianceOfDice > ci[1] && trueVarianceOfDice < ci[2])
}
                                        # should be close to 0.95
print(mean(trueValueCovered))
