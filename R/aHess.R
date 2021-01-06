aHess <- local({
#
# Analytic hessian (of the *negative* log likelihood).
#

hfun <- function(parz,ndata,ntop,zeta){
            -hess(parz[1],parz[2],ntop,zeta,ndata)
}

function(object) {
    if(!inherits(object,"mleHse"))
        stop("Argument \"object\" must be of class \"mleHse\".\n")
    parz  <- as.vector(object)
    ntop  <- attr(object,"ntop")
    zeta  <- attr(object,"zeta")
    ndata <- attr(object,"ndata")
    rslt  <- hfun(parz,ndata,ntop,zeta)
    rslt
}
})
