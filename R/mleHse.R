mleHse <- local({
    objFun <- function(par,x,ntop,zeta){
                -sum(log(dhse(i=x[!is.na(x)],alpha=par[1],
                              beta=par[2],ntop=ntop,zeta=zeta)))
              }

function (x,ntop,par0=NULL,eps=0.001,zeta=FALSE) {
# Maximum likelihood estimation of the shape parameters alpha
# and beta of the hse distribution.
    if(is.null(par0)) par0 <- meHse(x,ntop)
    cow  <- rep(eps,2)
    temp <- optim(par0,fn=objFun,method="L-BFGS-B",lower=cow,
                  ntop=ntop,zeta=zeta,x=x)
    rslt <- temp[["par"]]
    attr(rslt,"ntop") <- ntop
    attr(rslt,"zeta") <- zeta
    class(rslt) <- "mleHse"
    rslt
}
})
