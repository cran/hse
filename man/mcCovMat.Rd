\name{mcCovMat}
\alias{mcCovMat}
\title{
    Monte Carlo estimation of a covariance matrix.
}
\description{
    Calculate an estimate of the covariance matrix for the parameter
    estimates of an hse distribution via simulation.
}

\usage{
mcCovMat(object, nsim = 100, seed=NULL)
}
\arguments{
  \item{object}{
  An object of class \code{"mleHse"} as returned by the function
  \code{\link{mleHse}()}.
}
  \item{nsim}{
  Integer scalar.  The number of simulations to be used to produce
  the Monte Carlo estimate of the covariance matrix.
}
  \item{seed}{
  Integer scalar.  The seed for the random number generator.  If not
  specified it is randomly sampled from the sequence \code{1:1e5}.
}
}
\details{
    The procedure is to simulate \code{nsim} data sets (all of the
    same size as the data set to which \code{object} was fitted),
    based on the parameter estimates contained in \code{object}.

    From each such simulated data, parameter estimates are obtained.
    The covariance matrix of these latter parameter estimates is
    taken to be the required covariance matrix estimated.
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   It is an estimate of the covariance matrix of the parameter estimates.

   It has an attribute \code{"seed"} which is the seed that was used
   for the random number generator.  This is either the value of the
   argument \code{seed} or (if this argument was left \code{NULL}) the
   value that was randomly sampled from \code{1:1e5}.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
x <- X[[19]]$y
fit <- mleHse(x, ntop=5)
set.seed(42)
CM.m <- mcCovMat(fit,nsim=500) # Lots of simulations!
CM.a <- vcov(fit)
CM.n <- solve(nHess(fit,x))
cat("Monte Carlo:\n\n")
print(CM.m)
cat("Analytic:\n\n")
print(CM.a)
cat("Numeric:\n\n")
print(CM.n)
}

\concept{ covariance estimation }
\concept{ inference }
