					# the standard example, not used
standardEx <- c(2,2,4,6,2,6,4,5,4,6)

					# least squares unbiased estimator of the square of the population variance
lsepvs <- function(x) {
    m <- mean(x)
					# central second moment (caution: biased)
    c2 <- mean((x - m)^2)
					# central fourth moment (caution: biased)
    c4 <- mean((x - m)^4)
    n <- length(x)
					# least-squares unbiased estimator of the square of the population variance, contains a joint bias correction
    					# equals the U-statistic but in a computationally efficient form	
                                        # verified
    (1 + 1/2/(n-1) + 5/2/(n-2) + 9/2/(n-2)/(n-3)) * c2^2 - (1/(n-2) + 3/(n-2)/(n-3)) * c4
}


					# least square unbiased estimator of the variance of the usual unbiased sample variance
					# equals the U-statistic but in a computationally efficient form
varianceOfSampleVariance <- function(x) {
    v <- var(x)
    n <- length(x)
    					# expectation of square minus square of expectation, and analogously for the estimators
    					# the first term estimates its expectation, the second term the square of the expectation of the unbiased sample variance, i.e., the square of the population variance
    var(x)^2 - lsepvs(x)
}

                                        # the confidence interval for the population variance around the usual unbiased sample variance
					# using as standard deviation the square of the estimated variance of the usual unbiased sample variance, as estimated in the preceding function
varwci <- function(x, conf.level=0.95) {
    if (is.data.frame(x)) {
        stopifnot(dim(x)[2] == 1)
        x <- as.numeric(data.matrix(as.vector(x)))
    } else {stopifnot(is.atomic(x))}
    x <- as.vector(x)
    n <- length(x)
    stopifnot(n>=4)
    varsv <- varianceOfSampleVariance(x)
    if (varsv < 0) {
        warning("Sample size too small for estimation of the variance of the sample variance. Please use a larger sample.")
        r <- c(NA, NA)
    } else {
        t <- qt((1+conf.level)/2, df=n-1)
        v <- var(x)
        r <- c(v-t*sqrt(varsv), v + t*sqrt(varsv))
    }
    attributes(r) <- list(
        point.estimator=v,
        conf.level=conf.level,
        var.SampleVariance=max(0, varsv)
    )
    r
}
