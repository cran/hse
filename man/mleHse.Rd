\name{mleHse}
\alias{mleHse}
\title{
    Maximum likelihood estimates of hse parameters.
}
\description{
    Calculates maximum likelihood estimates of the \code{alpha} and
    \code{beta} parameters of an hse distribution.  Calls upon
    \code{\link{optim}()} with the \code{"L-BFGS-B"} method.
}
\usage{
mleHse(x, ntop, par0=NULL, eps=0.001, zeta=FALSE)
}
\arguments{
  \item{x}{
  A random sample from the hse distribution whose parameters are
  being estimated.  Missing values are \emph{allowed}.
}
  \item{ntop}{
  The \code{ntop} parameter of the hse distribution whose parameters are
  being estimated.  I.e. it is the maximum possible value of the distribution,
  whose values are integers between 1 and \code{ntop}, or between 0 and
  \code{ntop} if \code{zeta} (see below) is \code{TRUE}.
}
  \item{par0}{
  Optional starting values for the iterative estimation procedure.  A named
  vector with entries \code{alpha} and \code{beta}.
  If not supplied starting values are calculated using \code{\link{meHse}()}.
}
  \item{eps}{
  Positive scalar; a lower bound on the parameters (used by \code{optim()}
  in implementing the \code{"L-BFGS-B"} method.
}
  \item{zeta}{
  See \code{\link{dhse}()}.
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

  It is necessary to use the \code{"L-BFGS-B"}, with positive
  lower bounds, since non-positive values of the parmeters cause
  \code{dhse()} to produce \code{NaN}s.

  Missing values are removed from \code{x} before it is passed to
  \code{optim()}.  (Note that \code{dhse()} doesn't mind missing
  values but returns missing values when evaluated at them.
  This in turn produces a missing value for the log likelihood.)
}
\value{
  An object of class \code{"mleHse"}.  Such an object consists of a
  named vector with entries \code{"alpha"} and \code{"beta"}, which
  are the estimates of the corresponding parameters, with attributes
  \code{"ntop"} (the upper bound of the value of the distribution)
  and \code{"zeta"} (a logical scalar which is \code{TRUE} if the
  distribution uses zero origin indexing).
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
   \code{\link{hse}}
   \code{\link{meHse}()}
   \code{\link{optim}()}
}

\examples{
set.seed(42)
x <- rhse(500,3,5,2)
ests <- mleHse(x,2)  # Bad!  Mind you, 2 is a "bad" value for ntop!
# Get much better results using true parameter values
# as starting values; pity we can't do this in real life!
ests <- mleHse(x,2,par0=c(alpha=3,beta=5))
x <- rhse(500,3,5,20)
ests <- mleHse(x,20) # Pretty good.
# Binomial, n = 10, p = 0.3.
set.seed(42)
x <- rbinom(1000,10,0.3)
phi <- mleHse(x,10,zeta=TRUE)
p1  <- dbinom(0:10,10,0.3)
p2  <- dbinom(0:10,10,mean(x)/10)
p3  <- table(factor(x,levels=0:10))/1000
plot(phi,obsd=x,legPos=NULL,ylim=c(0,max(p1,p2,p3,
     dhse(0:10,phi[1],phi[2],10,zeta=TRUE))))
lines(0.2+(0:10),p1,col="orange",type="h",ylim=c(0,max(p1,p2)))
lines(0.3+(0:10),p2,col="green",type="h")
legend("topright",lty=1,col=c("red","blue","orange","green"),
       legend=c("hse","observed","true binomial","fitted binomial"),bty="n")
# Poisson mean = 5
set.seed(42)
x <- rpois(1000,5)
phi <- mleHse(x,14,zeta=TRUE) # max(x) = 13, take ntop = 1+13
p1 <- c(dpois(0:13,5),1-ppois(13,5))
lhat <- mean(x)
p2 <- c(dpois(0:13,lhat),1-ppois(13,lhat))
plot(phi,obsd=x,legPos=NULL,ylim=c(0,max(p1,p2,p3,
     dhse(0:14,phi[1],phi[2],14,zeta=TRUE))))
lines(0.2+0:14,p1,col="orange",type="h")
lines(0.3+(0:14),p2,col="green",type="h")
legend("topright",lty=1,col=c("red","blue","orange","green"),
       legend=c("hse","observed","true Poisson","fitted Poisson"),bty="n")
}
