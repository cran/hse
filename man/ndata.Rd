\name{ndata}
\alias{ndata}
\title{
    Retrieve the \code{"ndata"} attribute of an \code{"mleHse"}
    object.
}
\description{
    Retrieve the number of (non-missing) values in the data set
    to which an \code{"mleHse"} object was fitted.
}
\usage{
ndata(object)
}
\arguments{
  \item{object}{
   An object of class \code{"mleHse"} as returned by \code{\link{mleHse}()}.
}
}
\value{
   Integer scalar equal to the number of (non-missing) values in
   the data set to which \code{object} was fitted.
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
sapply(fitz,ndata)
}
\keyword{ utilities }
