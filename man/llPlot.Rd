\name{llPlot}
\alias{llPlot}
\title{
   Plot the log likelihood surface for the data.
}
\description{
   Plot, as a perspective plot or a contour plot, the log likelihood
   surface for the data set from which parameters are being estimated.
}
\usage{
llPlot(x, ntop, zeta, alim = c(0, 10), blim = c(0, 10),
       ngrid = c(100, 100), plotType = c("persp", "contour", "none"),
       theta = -30, phi = 40, ...)  }

\arguments{
  \item{x}{
  A vector of numeric data purportedly arising from an hse
  distribution.
}
  \item{ntop}{
  See \code{\link{mleHse}()} and \code{\link{dhse}()}.
}
  \item{zeta}{
  See \code{\link{mleHse}()} and \code{\link{dhse}()}.
}
  \item{alim}{
  Numeric vector of length 2; the range of \code{alpha} values
  over which the surface is to be plotted.
}
  \item{blim}{
  Numeric vector of length 2; the range of \code{beta} values
  over which the surface is to be plotted.
}
  \item{ngrid}{
  The dimensions of the grid of \code{(alpha,beta)} values at
  which the log likelihood is to be evaluated in order to plot
  the surface.  Note that \code{ngrid} may be supplied as an
  integer scalar, in which case it is replicated to a vector
  of length 2.
}
  \item{plotType}{
  Character string specifying the nature of the plot to be
  produced.  If it is \code{"none"} then no plot is produced.
  The value returned may be plotted at a later occasion.
}
  \item{theta}{
  An argument to be passed to \code{\link{persp}()}.
  Ignored unless \code{plotType} is \code{"persp"}.
}
  \item{phi}{
  An argument to be passed to \code{persp()}.
  Ignored unless \code{plotType} is \code{"persp"}.
}
  \item{\dots}{
  Other arguments that may be passed to \code{persp()}
  or to \code{\link{contour}()}
}
}
\details{
  This function could conceiveably be useful in diagnosing
  problems with parameter estimation should these arise.
}
\value{
  A list with entries
  \item{x}{The vector of \code{alpha} values over which
   the surface is to be plotted.  There are \code{ngrid[1]}
   such values, ranging from \code{alim[1]} to \code{alim[2]}.}
  \item{y}{The vector of \code{beta} values over which
   the surface is to be plotted.  There are \code{ngrid[2]}
   such values, ranging from \code{blim[1]} to \code{blim[2]}.}
  \item{z}{An \code{ngrid[1] x ngrid[2]} numeric matrix,
  specifying the surface.  the value of \code{z[i,j]} is
  \code{ll(x[i],y[j]} where \code{ll()} is the log likelihood
  function.}
  \item{ab}{A data frame with columns \code{"alpha"} and
  \code{"beta"} and \code{ngrid[1]*ngrid[2]} rows.  It
  is formed by applying \code{expand.grid()} to the \code{x}
  and \code{y} entries of this list.}
  \item{fab}{A numeric vector of length \code{ngrid[1]*ngrid[2]}.
  Its \code{i}th value is the log likelihood evaluated at
  the \code{i}th row of \code{ab}.  Its entries are the
  same as the entries of \code{z}.}

  There is obviously considerable redundancy in the returned
  value.

  The names \code{x} and \code{y} are used for the first two
  entries of this list so as to conform to the names of the
  arguments of \code{persp()} and \code{contour}.

  If \code{plotType} is \code{"persp"} or \code{"contour"} the
  value is returned invisibly.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
  \code{link{mleHse}()}
  \code{link{persp}()}
  \code{link{contour}()}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
x <- X[[19]]$y
srf <- llPlot(x,5,FALSE,alim=c(0.5,0.7),blim=c(0.2,0.4),plotType="c")
\dontrun{
if(require(rgl)) {
   with(srf,plot3d(ab$alpha,ab$beta,fab)
# Allows dynamic rotation of the surface.
}
}
# Negative (!) parameters.
set.seed(42)
xs <- rhse(100,-1,-1,5)
fit <- mleHse(xs,5)
llPlot(xs,5,FALSE,alim=c(-4,2),blim=c(-4,2),plotType="c",
       main="log likelihood contours")
points(fit[1],fit[2],pch=20,col="red")
points(-1,-1,pch=20,col="blue")
legend("topright",pch=20,col=c("red","blue"),
       legend=c("estimate","true value"),bty="n")
}

\keyword{hplot}
