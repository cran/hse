\name{meHse}
\alias{meHse}
\title{
    Moment estimates of hse parameters.
}
\description{
    Calculates \bold{rough} moment estimates of the \code{alpha} and
    \code{beta} parameters of an hse distribution.  These estimates
    are mainly useful as starting values for maximum likelihood
    estimation.
}
\usage{
meHse(x, ntop)
}
\arguments{
  \item{x}{
  A random sample from the hse distribution whose parameters are
  being estimated.  Missing values are \emph{allowed}.
}
  \item{ntop}{
  The \code{ntop} parameter of the hse distribution whose parameters are
  being estimated.  I.e. it is the maximum possible value of the distribution,
  whose values are integers between 1 and \code{ntop}.
}
}
\details{
The \code{ntop} parameter must be supplied; it is not estimated.
If the the generating mechanism from which the observed data \code{x}
arose has a (known) theoretical least upper bound, then \code{ntop}
should probably be set equal to this upper bound.  If the data
are theoretically unbounded, then \code{ntop} should probably
be set equal to \code{1 + max(x)}.  In this case \eqn{\Pr(X =
\textrm{ntop})}{Pr(X=ntop)} should probably be interpreted
as \eqn{\Pr(X \geq \textrm{ntop})}{Pr(X >= ntop)}.  Otherwise
\code{ntop} should should probably be set equal to \code{max(x)}.
The choice depends on circumstances and is up to the user.

The estimates are \emph{very} rough when \code{ntop} is \dQuote{small}.
They get better for larger values of \code{ntop}.

The code removes missing values from \code{x} before commencing
calculations.

The data are treated as if zero origin indexing were used, which
might not be the case.  This doesn't really matter since this estimation
method is so rough anyway! 
}
\value{
A named vector with entries \code{alpha} and \code{beta} which are the
estimates of the corresponding parameters.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
   \code{\link{hse}}
   \code{\link{mleHse}()}
}

\examples{
set.seed(42)
x <- rhse(500,3,5,2)
ests <- meHse(x,2)  # Bad!
x <- rhse(500,3,5,20)
ests <- meHse(x,20) # Pretty good.
}
