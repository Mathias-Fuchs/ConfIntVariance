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

                                        # least-squares unbiased (and therefore unique, thus "the") estimator of E[(X-m)^4], the centralized but non-standardized kurtosis
kurt <- function(x) {
    m <- mean(x)
    c2 <- mean((x - m)^2)
    c4 <- mean((x - m)^4)
    n <- length(x)
    (3/2/(n-1) + 6/(n-2) - 27/2/(n-3)) * c2^2 + (1+(1/(n-1)-6/(n-2)+9/(n-3))) * c4
}

					# equals the U-statistic but in a computationally efficient form
varianceOfSampleVariance <- function(x) {
    v <- var(x)
    n <- length(x)
    					# expectation of square minus square of expectation, and analogously for the estimators
    					# the first term estimates its expectation, the second term the square of the expectation of the unbiased sample variance, i.e., the square of the population variance
    var(x)^2 - lsepvs(x)
}

                                        # alternative way to estimate the variance of the sample variance, closer to the formula kurt/n - sigma^4*(n-3)/n/(n-1) from the literature, for instance Cramer.
                                        # gives the same result as the preceding function
varianceOfSampleVariance2 <- function(x) {
    n <- length(x)
    kurt(x)/n - lsepvs(x)*(n-3)/n/(n-1)
}


                                        # yet another formula for the same object, gives the same result
varianceOfSampleVariance3 <- function(x) {
    m <- mean(x)
    c2 <- mean((x - m)^2)
    c4 <- mean((x - m)^4)
    n <- length(x)
    ( (3/2/(n-1) + 6/(n-2) - 27/2/(n-3))/n -  (1 + 1/2/(n-1) + 5/2/(n-2) + 9/2/(n-2)/(n-3)) * (n-3)/n/(n-1)) * c2^2 + ( (1+(1/(n-1)-6/(n-2)+9/(n-3)))/n+(1/(n-2) + 3/(n-2)/(n-3))
*(n-3)/n/(n-1)) * c4
}

                                        # yet another formula for the same object, gives the same result
varianceOfSampleVariance4 <- function(x) {
    m <- mean(x)
    c2 <- mean((x - m)^2)
    c4 <- mean((x - m)^4)
    n <- length(x)
    ( 2/(n - 2) + 3/(2* (n - 1)) + 1/(n - 1)^2 - 9/(2 *(n - 3))) * c2^2 + (3/(n - 3) - 2/(n - 2)) * c4
}

                                        # yet another formula for the same object, gives the same result
varianceOfSampleVariance5 <- function(x) {
    n <- length(x)
    (1/(2 *(n - 2)) + 1/(2* n) - 2/(n - 3)) * var(x)^2  + (3/(n - 3) - 2/(n - 2)) * mean((x-mean(x))^4)
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
    v <- var(x)
                                        # might also take the equivalent slower variants varianceOfSampleVariance"i" where i=1...5
                                        # varsv <- varianceOfSampleVariance(x) etc.
                                        # the following inline version is the quickest variant
    varsv <- (1/(2 *(n - 2)) + 1/(2* n) - 2/(n - 3)) * v^2  + (3/(n - 3) - 2/(n - 2)) * mean((x-mean(x))^4)
    if (varsv < 0) {
        warning("Sample size too small for estimation of the variance of the sample variance. Please use a larger sample.")
        r <- c(NA, NA)
    } else {
        t <- qt((1+conf.level)/2, df=n-1)
        r <- c(v-t*sqrt(varsv), v + t*sqrt(varsv))
    }
    attributes(r) <- list(
        point.estimator=v,
        conf.level=conf.level,
        var.SampleVariance=max(0, varsv)
    )
    r
}
