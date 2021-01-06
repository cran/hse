\name{plotHse}
\alias{plotHse}
\title{
    Plot an hse distribution.
}
\description{
    Plots the probabilities of a specified hse distributon.
}
\usage{
plotHse(alpha, beta, ntop, zeta, ..., tikx = NULL, xlim = NULL,
        ylim = NULL, xlab = NULL, ylab = NULL, main = "")

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
  \item{\dots}{
  Extra arguments that are passed to the \code{plot()} function.
}
  \item{tikx}{
  (Optional) vector of locations of the tick marks on the \code{x}-axis.
}
  \item{xlim}{
  The \code{x}-limits of the plot.  (See \code{\link{plot.default}()}.)
}
  \item{ylim}{
  The \code{y}-limits of the plot.  (See \code{\link{plot.default}()}.)
}
  \item{xlab}{
  A label for the \code{x}-axis. (See \code{\link{plot.default}()}.)
}
  \item{ylab}{
  A label for the \code{y}-axis. (See \code{\link{plot.default}()}.)
}
  \item{main}{
  An overall title for the plot. (See \code{\link{plot.default}()};
  see also \code{\link{title}()}.)
}
}
\value{
None. A plot is produced as a side-effect.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
   \code{\link{plot.mleHse}()}
}
\examples{
plotHse(2,3,14,FALSE,main="An exempular plot")
plotHse(2,3,14,TRUE,col="red",xlab="count",main="A communist plot")
plotHse(0.1,3,14,TRUE,col="blue",main="A royal plot")
plotHse(0.1,0.3,14,TRUE,col="green",main="An ecological plot")
plotHse(2,3,14,FALSE,xlim=c(0,15))
plotHse(2,3,14,FALSE,xlim=c(0,15),tikx=3*(0:5))
par(mfrow=c(2,1))
plotHse(2,2,5,FALSE,main=bquote(paste(alpha == 2,", ",beta == 2)),col="red")
plotHse(-2,-2,5,FALSE,main=bquote(paste(alpha == -2,", ",beta == -2)),col="blue")

}

\keyword{ hplot }
