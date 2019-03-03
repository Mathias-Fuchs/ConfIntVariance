
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
    if (is.data.frame(x)) {
        stopifnot(dim(x)[2] == 1)
        x <- as.numeric(data.matrix(as.vector(x)))
    }
    else stopifnot(is.atomic(x))
    n <- length(x)
    stopifnot(n>=4)
    t <- qt((1+conf.level)/2, df=n-1)
    v <- var(x)
    varsv <- varianceOfSampleVariance(x)
    if (varsv < 0) {
        warning("Sample size too small for estimation of the variance of the sample variance")
        return(c(NA, NA))
    }
    c(v-t*sqrt(varsv), v + t*sqrt(varsv))
}
