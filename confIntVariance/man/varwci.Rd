\name{g}
\alias{g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  varwci
%%  ~~function to do ... ~~
}
\description{
  Surround the univariate variance estimator of the function var with a
  confidence interval, not assuming normality
}
\usage{
varwci(s)
}
%- maybe also 'usage' for other objects documented here.

\value{
Returns a vector with two entries: the lower and the upper bound of the
confidence interval.
}

\references{
www.mathiasfuchs.de/b3.html
}

\author{
Mathias Fuchs
}

\note{
The underlying theory is that of U-statistics
}

\section{Warning }{
On very small sample sizes, the result is NA because there is
insufficient information on the variance estimation
}

\examples{
# throw a dice 100 times
s <- sample(6, 100, replace=TRUE)
# the standard point estimator for the variance
print(var(s))

# contains the true value 2.9166 with a probability of 95 percent.
print(varwci(s))
}

\keyword{variance estimation}
\keyword{confidence interval}
