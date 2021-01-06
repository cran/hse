nHess <- local({
#
# Numerical (finite differencing based) approximation to
# the hessian, as effected by optim().
#

Fun <- function(par,x,ntop,zeta){
#
# Note that this is the negative log likelihood as *minimised*
# by mleHse().  Hence the (estimated) covariance matrix is the
# inverse of the (approximate) hessian returned by this function
# and not of the negative of this approximate hessian.
#
    -sum(log(dhse(x=x[!is.na(x)],alpha=par[1],
                 beta=par[2],ntop=ntop,zeta=zeta)))
}

function(object,x,silent=TRUE) {
    if(!inherits(object,"mleHse"))
        stop("Argument \"object\" must be of class \"mleHse\".\n")
    ntop <- attr(object,"ntop")
    zeta <- attr(object,"zeta")
    parz <- as.vector(object)
    hiss <- try(optimHess(parz,fn=Fun,ntop=ntop,zeta=zeta,x=x),
                silent=silent)
    if(inherits(hiss,"try-error")) {
        whinge <- paste0("Function optimHess failed.  Perhaps try\n",
                         " using functions aHess() or mcCovMat().\n")
        stop(whinge)
    }
    ndata <- sum(!is.na(x))
    rslt <- hiss
    rslt
}
})
