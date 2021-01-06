\name{varHse}
\alias{varHse}
\alias{varHse.mleHse}
\alias{varHse.default}
\title{
    Variance of an hse distribution.
}
\description{
    Calculate the variance of a random variable having an hse
    distribution.
}
\usage{
varHse(ao,...)
\method{varHse}{mleHse}(ao,...)
\method{varHse}{default}(ao, beta, ntop, zeta=FALSE,...)
}
\arguments{
  \item{ao}{
  For the \code{"mleHse"} method this argument is an object of class
  \code{"mleHse"} as returned by \code{\link{mleHse}()}.  For the
  default method it is a numeric scalar playing the role of
  \code{alpha} (see \code{\link{dhse}()}).
}
  \item{beta}{
  See \code{\link{dhse}()}.
}
  \item{ntop}{
  See \code{\link{dhse}()}.
}
  \item{zeta}{
  See \code{\link{dhse}()}.
}
  \item{\dots}{
  Not used.
}
}
\details{
   For the \code{"mleHse"} method, the single argument should really
   be called (something like) \dQuote{\code{object}} and for the
   default method the first argument should be called \code{alpha}.
   However the argument lists must satisfy
   the restrictions that \dQuote{\emph{A method must have all the
   arguments of the generic, including \dots if the generic does.}}
   and \dQuote{\emph{A method must have arguments in exactly the
   same order as the generic.}}
   
   For the \code{"mleHse"} method, the values of \code{alpha},
   \code{beta}, \code{ntop} and \code{zeta} (passed to \code{dhse()})
   are extracted from the attributes of \code{ao}.

   The variance of an hse distribution is theoretically intractable
   but is readily calculable numerically as \deqn{\sum (x - \mu)^2
   \times \Pr(X=x)}{sum((x-mu) * Pr(X=x))}, where \eqn{\mu}{mu}
   is the expected value of the given distribution.
}
\value{
Numeric scalar equal to the variance of an hse distributed
random variables with the given parameters.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{dhse}()} \code{\link{expValHse}()}
}
\examples{
   varHse(3,4,15)
   varHse(3,4,15,TRUE)
   X   <- hmm.discnp::Downloads
   fit <- mleHse(X,ntop=15,zeta=TRUE)
   varHse(fit)
}
\keyword{math}
\keyword{univar}
\concept{variance}
