\name{nHess}
\alias{nHess}
\title{
   Numerical hessian calculation.
}
\description{
   Calculate an approximation to the hessian of the \bold{negative}
   log likelihood of an hse distribution via a numerical (finite
   differencing based) procedure as effected by optim() or
   optimHess().
}
\usage{
nHess(object, x, silent=TRUE)
}
\arguments{
  \item{object}{
  An object of class \code{"mleHse"} as returned by the
  function \code{\link{mleHse}()}.
}
  \item{x}{
  Numeric vector of non-negative integer data, presumably the
  data set on the basis of which \code{object} was calculated.
}
  \item{silent}{
  Logical scalar.  If the call to \code{\link{optimHess}()} throws
  an error, should the error message be suppressed?  (A possibly
  less informative warning will be issued in any case.)
}
}
\details{
   It is up to the user to make sure that \code{object} and
   \code{x} are \dQuote{mutually compatible}, i.e. are appropriately
   paired up.

   Note that this function calculates the hessian of the
   \bold{negative} log likelihood of an hse distribution,
   as \emph{minimised} by \code{optim()}.  Hence its inverse
   is an estimate of the covariance matrix of the parameter estimates.
   (Do \emph{not} take the negative of this hessian before inverting
   it to get the desired covariance matrix.

   This function is mainly present to investigate possible differences
   between the numerical approximation to the hessian, which is what
   \code{optim()} uses in its maximisation procedure, and the analytic
   form of the hessian. % Which may be more accurate, particularly
   % for small values of "ndata".
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
   \code{\link{aHess}()}
   \code{\link{mleHse}()}
   \code{\link{optim}()}
   \code{\link{optimHess}()}
}

\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
x <- X[[19]]$y
fit <- mleHse(x, ntop=5)
H   <- nHess(fit,x)
print(solve(H)) # Compare with ...
print(vcov(fit))
}

\concept{ hessian }
\concept{ covariance estimation }
\concept{ inference }
