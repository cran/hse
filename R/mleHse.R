mleHse <- local({
    objFun <- function(par,x,ntop,zeta){
                -sum(log(dhse(x=x[!is.na(x)],alpha=par[1],
                              beta=par[2],ntop=ntop,zeta=zeta)))
              }

    gfun <- function(par,x,ntop,zeta){
                -grad(x[!is.na(x)],par[1],par[2],ntop,zeta)
            }

    hfun <- function(par,ndata,ntop,zeta){
                -hess(par[1],par[2],ntop,zeta,ndata)
            }

function (x,ntop,zeta=FALSE,par0=NULL,UB=10) {
# Maximum likelihood estimation of the shape parameters alpha
# and beta of the hse distribution.
    if(is.null(par0)) par0 <- pmin(meHse(x,ntop),UB)
    temp <- optim(par0,fn=objFun,gr=gfun,method="BFGS",
                  ntop=ntop,zeta=zeta,x=x)
    rslt <- temp[["par"]]
    attr(rslt,"ntop") <- ntop
    attr(rslt,"zeta") <- zeta
    attr(rslt,"log.like") <- -temp$value
    ndata <- sum(!is.na(x))
    attr(rslt,"ndata") <- ndata
    H  <- hfun(rslt,ndata,ntop,zeta)
    CM <- try(solve(H))
    if(inherits(CM,"try-error")) {
        attr(rslt,"covMat") <- NA
        whinge <- paste0("Hessian appears to be singular. Setting\n",
                         "  \"covMat\" attribute to be NA.\n")
        warning(whinge)
    } else {
        attr(rslt,"covMat") <- CM
    }
    class(rslt) <- "mleHse"
    rslt
}
})
