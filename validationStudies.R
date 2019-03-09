require(ConfIntVariance)
                                        # Confirm that the variance of the sample variance is unbiased even on small samples, for instance tossing a dice with n = 5, known to have:
                                        # true central fourth moment mu_4 is 707/48=14.72917, or empirically mean(((1:6) - 3.5)^4)

mu4 <- 707/48
variance <- 35/12
n <- 20
                                        # Cramér's formula for the true variance of the sample variance
                                        # We could of course also estimate that value empirically
print(trueVarianceOfSampleVariance <- mu4/n - variance^2*(n-3)/n/(n-1))
                                        # Confirms that the estimator is unbiased: t-test confidence interval contains the true value
print(t.test(sapply(1:1e3, function(i) attr(varwci(sample(6, n, replace=TRUE)), "var.SampleVariance"))))



                                        # the same for the normal
                                        # mu4 = 3
mu4 <- 3
variance <- 1
n <- 20
print(trueVarianceOfSampleVariance <- mu4/n - variance^2*(n-3)/n/(n-1))
                                        # confirms that the estimator is unbiased: t-test confidence interval contains the true value
print(t.test(sapply(1:1e5, function(i) attr(varwci(rnorm(n)), "var.SampleVariance"))))

                                        # and so on







N <- 1e4
trueValueCovered <- sapply(
    1:N,
    function(i) {
                                        # throw a dice 100 times
        x <- sample(6, 100, replace=TRUE)
                                        # compute our confidence interval
        ci <- varwci(x)
                                        # We know that the true variance of the dice is 35/12 = 2.916666...
                                        # Record the boolean whether the confidence interval contains the correct value
        trueValueCovered[i] <- (35/12 > ci[1] && 35/12 < ci[2])
    }
)

                                        # Result of simulation study: Close to 0.95, works, confirms validity
print(paste("Simulated coverage probability:", mean(trueValueCovered)))

                                        # compare with the competitors
                                        # competitor one: function varTest in package EnvStats
                                        # competiror two: parameters of the structual equation model in package lavaan
require(EnvStats)
require(lavaan)

lavaanCI <- function(x, conf.level) {
    df <- as.data.frame(x)
    names(df) <- "x"
    model <- 'x ~~ x'
    fit <- sem(model, data=df, likelihood = "wishart" )
    pe <- parameterEstimates(fit, level=conf.level)
    c(pe$ci.lower, pe$ci.upper)
}
    
N <- 1e4
trueValueCovered <- sapply(
    1:N,
    function(i) {
        x <- sample(6, 100, replace=TRUE)
        proposedMethod <- varwci(x, conf.level=0.95)
                                        # competitor one: ci from function varTest
        literatureMethodCS <- varTest(x, conf.level=0.95)$conf.int
                                        # competitor two: ci from structual equation model
        literatureMethodLH <- lavaanCI(x, conf.level=0.95)
        covered1 <- (35/12 > proposedMethod[1] && 35/12 < proposedMethod[2])
        covered2 <- (35/12 > literatureMethodCS[1] && 35/12 < literatureMethodCS[2])
        covered3 <- (35/12 > literatureMethodLH[1] && 35/12 < literatureMethodLH[2])        
        c(
            covered1, covered2, covered3,
            proposedMethod[2] - proposedMethod[1], literatureMethodCS[2] - literatureMethodCS[1], literatureMethodLH[2] - literatureMethodLH[1]
        )
    }
)
res <- rowMeans(trueValueCovered)
                                        # close to the nominal 95%.
print(paste("Coverage probability of proposed method", res[1]))
                                        # close to 99%, i.e., on the conservative side. Confidence interval too big.
print(paste("Coverage probability of varTest", res[2]))
                                        # close to 99%, i.e., on the conservative side. Confidence interval too big.
print(paste("Coverage probability of lavaan", res[3]))
print(paste("Average length of proposed confidence interval", res[4]))
print(paste("Average length of chi-square confidence interval", res[5]))
print(paste("Average length of structual equation confidence interval", res[6]))
