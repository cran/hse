\name{vcov.mleHse}
\alias{vcov.mleHse}
\title{
   Retrieve the covariance matrix from an \code{"mleHse"} object.
}
\description{
   Extract the covariance matrix attribute an object of class
   \code{"mleHse"}.  I.e. obtain the estimated covariance matrix
   of the maximum likelihood estimates of the parameters of an
   hse distribution.
}
\usage{
   \method{vcov}{mleHse}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleHse"} as returned by \code{\link{mleHse}()}.
}
  \item{\dots}{
   Not used.
}
}
\details{
   The estimated covariance matrix is in effect the inverse of the
   observed Fisher Information matrix.
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   It is an estimate of the covariance matrix of the parameter estimates.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{mleHse}()}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
fitz <- lapply(X,function(x){mleHse(x$y,ntop=5)})
lapply(fitz,vcov)
}
\keyword{ utilities }
