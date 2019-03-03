varwci <- function(s) {
    n <- length(s)
    N <- matrix(4, n)
    N <- 0
    for (i in seq(2, n)) N[1, i] = N[1, i - 1] + s[i]
		for (size_t i = 1; i < B; i++) for (int j = 1; j < 4; j++) N[i + B * j] = gsl_vector_get(resamplingResults, i) * N[i - 1 + B * (j - 1)] + N[i - 1 + B * j];




}
