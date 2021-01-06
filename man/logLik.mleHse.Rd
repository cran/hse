\name{logLik.mleHse}
\alias{logLik.mleHse}
\title{
   Retrieve the (maximised) log likelihood from an \code{"mleHse"} object.
}
\description{
   Extract the log likelihood attribute an object of class
   \code{"mleHse"}.  I.e. obtain the maximum log likelihood
   in respect of the estimation of the parameters of an hse
   distribution.
}
\usage{
   \method{logLik}{mleHse}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleHse"} as returned by \code{\link{mleHse}()}.
}
  \item{\dots}{
   Not used.
}
}
\value{
   A numeric scalar equal to the maximum log likelihood for the parameters
   of an hse distribution.
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
sapply(fitz,logLik)
}
\keyword{ utilities }
