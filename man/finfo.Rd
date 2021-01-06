\name{finfo}
\alias{finfo}
\title{
    Fisher information.
}
\description{
    Compute the Fisher information for an hse distribution, given
    the parameters of that distribution and a specified number
    of observations.  This is in effect the hessian of the
    \bold{negative} log likelihood of the distribution.
}
\usage{
finfo(alpha, beta, ntop, ndata, zeta = FALSE)
}
\arguments{
  \item{alpha}{
  See \code{\link{dhse}()}.
}
  \item{beta}{
  See \code{\link{dhse}()}.
}
  \item{ntop}{
  See \code{\link{dhse}()}.
}
  \item{ndata}{
  The number of observations for which the Fisher information
  is being determined.
}
  \item{zeta}{
  See \code{\link{dhse}()}.
}
}
\details{
   This function differs from \code{\link{aHess}()} only in that
   its arguments are prescribed \dQuote{individually} rather than
   being extracted from an \code{"mleHse"} object.  This allows
   \code{finfo()} to be applied to \dQuote{true} parametersi (where
   these are known) rather than estimated ones.

   Note that for \code{finfo()} the number of observations must
   be supplied explicitly, whereas for \code{aHess()} this number
   is extracted from the \code{object} argument.
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   Its inverse is an estimate of the covariance matrix of the
   parameter estimates.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{link{aHess}()}
  \code{link{nHess}()}
  \code{link{mleHse}()}
}
\examples{
print(finfo(0.6,0.3,5,54))
X   <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X   <- split(X,f=with(X,interaction(locn,depth)))
x   <- X[[19]]$y
fit <- mleHse(x, ntop=5)
alpha <- fit["alpha"]
beta  <- fit["beta"]
ntop  <- attr(fit,"ntop")
zeta  <- attr(fit,"zeta")
ndat  <- ndata(fit)
print(finfo(alpha,beta,ntop,ndat,zeta))
print(aHess(fit)) # Same
}

\concept{ Fisher information }
\concept{ covariance estimation }
\concept{ inference }
