\name{varHse}
\alias{varHse}
\title{
    Variance of the hse distribution.
}
\description{
    Calculate the (theoretical or population) variance of a
    random variable having an hse distriution.
}
\usage{
varHse(alpha,beta, ntop, zeta=FALSE)
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
  \item{zeta}{
  See \code{\link{dhse}()}.
}
}
\details{
   The variance of an hse distribution is theoretically
   intractable but is readily calculable numerically as
   \deqn{\sum (x-\mu)^2 \times \Pr(X=x)}{sum((x-mu)^2 * Pr(X=x))}
   where \eqn{\mu}{mu} is the (theoretical) mean of the
   distribution.
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
}
