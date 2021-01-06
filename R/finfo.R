finfo <- local({
#
# Fisher information (equal to the hessian (of the *negative*
# log likelihood).

hfun <- function(parz,ndata,ntop,zeta){
            -hess(parz[1],parz[2],ntop,zeta,ndata)
}

function(alpha,beta,ntop,ndata,zeta=FALSE) {
    parz <- c(alpha,beta)
    rslt <- hfun(parz,ndata,ntop,zeta)
    rslt
}
})
