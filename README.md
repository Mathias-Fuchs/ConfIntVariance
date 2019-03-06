# ConfIntVariance
An R package providing one function varwci ("variance with confidence interval") to surround the standard sample variance of the function "var" with a confidence interval containing the true variance with a probability of 95%.

The validity of the confidence interval is confirmed by a coverage probability simulation study given in the examples in the documentation.

# How does it work?
It works by implementing an unbiased estimator of the variance of the sample variance, and then setting up the standard studentized confidence interval using the square of that unbiased estimator. So far, the strategy is the same as the one used to obtain the confidence interval for the mean, as it is implemented in the t.test function in R, for instance.

There exists a closed form for the variance of the sample variance but it involves the square of the underlying distribution's variance. The difficulty is to estimate that square of the variance. There is a U-statistic estimator for it but a naive implementation would be of complexity O(n^4) where n is the sample size which is prohibitively costly. In this package, we solve the problem by reducing the computation to complexity O(n) using a dynamic programming approach. See [https://mathiasfuchs.de/b3.html](this blog post) for a leisurely overview of the derivation of how to estimate the variance of the sample variance.

# Related work
There are implementations of confidence intervals but they all suffer from a drawback, either
* they assume normality of the underlying distribution, basically replacing the fourth moment with a known value.
* or they use heuristics such as bootstrapping etc., which are only approximately valid.
* or they cannot deal with ties in the data.

Here, we implemement a confidence interval that relies on the universally best (in the least-mean-squared sense) estimator of the variance of the sample variance, it being the U-statistic estimator. So, the validity of the approach is mathematically proven.

# Reference
The underlying U-statistics variance estimator is a special case of the one underlying this publication [http://dx.doi.org/10.1080/15598608.2016.1158675](http://dx.doi.org/10.1080/15598608.2016.1158675), freely available [https://epub.ub.uni-muenchen.de/27656/7/TR.pdf](here).

# Validation
One can easily verify the validity of the confidence interval by choosing a population aka distribution (for instance, a dice) computing the true variance either theoretically or by taking the sample variance of a very large sample (with n a few hundred millions), drawing many samples (all of the same size or not), and checking that the confidence interval covers the true value in about 95% of all cases. For instance, in the dice example, the true variance is 35 / 12 = 2.91666, and the following code confirms the validity:

```R
require(ConfIntVariance)
N <- 1e4
trueValueCovered <- rep(NA, N)
for (i in 1:N) {
    if (i %% 1e2 == 0) print(i)
                                        # throw a dice 100 times
    x <- sample(6, 100, replace=TRUE)
                                        # compute our confidence interval
    ci <- varwci(x)
                                        # We know that the true variance of the dice is 35/12 = 2.916666...
                                        # Record the boolean whether the confidence interval contains the correct value
    trueValueCovered[i] <- (35/12 > ci[1] && 35/12 < ci[2])
}

                                        # Result of simulation study: should be close to 0.95
print(mean(trueValueCovered))
```

# Installation
Download the relase file ConfIntVariance_1.0.tar.gz from 

[https://github.com/Mathias-Fuchs/ConfIntVariance/releases/tag/1.01](https://github.com/Mathias-Fuchs/ConfIntVariance/releases/tag/1.01)

and install with install.packages("ConfIntVariance_1.0.tar.gz")

# Documentation
In R, call the help with ?varwci

