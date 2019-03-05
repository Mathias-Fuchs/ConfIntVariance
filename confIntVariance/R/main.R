
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

lsesv <- function(x) {
    v <- var(x)
    n <- length(x)
    elSym <- productsOfDisjointTuples(x)
                                        # triple-checked formula
    (2/n/(n-1) * elSym[2]^2 - 4/n/(n-2) * elSym[1] * elSym[3] + 4/(n-2)/(n-3) * elSym[4])
}

lsesvExp <- function(x) {
    stopifnot(length(x)==6)
    p1 <- sum(x)
    p2 <- sum(x^2)
    p3 <- sum(x^3)
    p4 <- sum(x^4)
    (1/360)*p1^4 +(-1/30)*p1^2*p2 +(7/120)*p2^2 +(1/18)*p1*p3 +(-1/12)*p4
}

varianceOfSampleVariance <- function(x) {
    v <- var(x)
    n <- length(x)
    p1 <- productsOfDisjointTuples(x)
    p2 <- productsOfDisjointTuples(x^2)
    sv <- lsesv(x)
                                        # give the same value, up to numerical inaccuracies
    k1 <- var(x)^2 - p2[2] / choose(n, 2) + 4/n/(n-1)/(n-2)*(p1[3]*sum(x) - 4 * p1[4]) - p1[4]/choose(n, 4)
    k2 <- var(x)^2 - sv
    k1
}


x <-  (1:6)^3
print(
    mean(
        apply(
            combn(6, 4),
            2,
            function(col) {
                mean(
                                                   c(
                                                   (x[col[1]] - x[col[2]])^2 * (x[col[3]] - x[col[4]])^2,
                                                   (x[col[1]] - x[col[3]])^2 * (x[col[2]] - x[col[4]])^2,
                                                   (x[col[1]] - x[col[4]])^2 * (x[col[2]] - x[col[3]])^2
                                                   )
                )
                }
                 )
           )
      /4
) 






                                        # the confidence interval for the variance
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
        warning("Sample size too small for estimation of the variance of the sample variance")
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
