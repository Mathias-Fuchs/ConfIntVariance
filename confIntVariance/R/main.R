
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

standardEx <- c(2,2,4,6,2, 6,4,5,4,6)

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

varianceOfSampleVariance <- function(x) {
    v <- var(x)
    n <- length(x)
 #   p1 <- productsOfDisjointTuples(x)
 #   p2 <- productsOfDisjointTuples(x^2)
                                        # give the same value, up to numerical inaccuracies
 #   k1 <- var(x)^2 - p2[2] / choose(n, 2) + 4/n/(n-1)/(n-2)*(p1[3]*sum(x) - 4 * p1[4]) - p1[4]/choose(n, 4)
 #   k2 <- var(x)^2 - lsepvs(x)
    k3 <- var(x)^2 - lsepvs2(x)
#    print(paste(k1, k2, k3))
    k3

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
    v <- var(x)
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
