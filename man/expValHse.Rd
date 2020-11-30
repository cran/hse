\name{expValHse}
\alias{expValHse}
\title{
    Expected value of the hse distribution.
}
\description{
    Calculate the expected value (theoretical mean) of a
    random variable having an hse distriution.
}
\usage{
expValHse(alpha, beta, ntop, zeta=FALSE)
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
   The expected value of an hse distribution is theoretically
   intractable but is readily calculable numerically as
   \deqn{\sum x \times \Pr(X=x)}{sum(x * Pr(X=x))}.
}
\value{
Numeric scalar equal to the expected value of an hse distributed
random variables with the given parameters.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{dhse}()} \code{\link{varHse}()}
}
\examples{
   expValHse(3,4,15)
}
