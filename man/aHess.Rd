\name{aHess}
\alias{aHess}
\title{
   Analytic hessian.
}
\description{
   Compute the hessian of the \bold{negative} log likelihood of an
   hse distribution from an analytic expression for this quantity.
}
\usage{
aHess(object)
}
\arguments{
  \item{object}{
  An object of class \code{"mleHse"} as returned by the
  function \code{\link{mleHse}()}.
}
}
\details{
  This function is essentially the same as \code{\link{finfo}()} and
  differs from it only in that is desiged to act up \code{"mleHse"}
  objects, from which (estimates of) the relevant parameters are
  extracted.
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
   \code{\link{nHess}()}
   \code{\link{finfo}()}
   \code{\link{mleHse}()}
}

\examples{
X   <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X   <- split(X,f=with(X,interaction(locn,depth)))
x   <- X[[19]]$y
fit <- mleHse(x, ntop=5)
H   <- aHess(fit)
print(solve(H)) # Equal to ...
print(vcov(fit))
}

\concept{ hessian }
\concept{ covariance estimation }
\concept{ inference }
