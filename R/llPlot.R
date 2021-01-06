llPlot <- local({

Fun <- function(par,x,ntop,zeta){
#
# Note that this functon plots the log likelihood surface. as
# *minimised* by The mleHse() function *minimises* the *negative*
# log likelihood.
#
    sum(log(dhse(x=x[!is.na(x)],alpha=par[1],
                 beta=par[2],ntop=ntop,zeta=zeta)))
}

function(x,ntop,zeta,alim=c(0,10),blim=c(0,10),ngrid=c(100,100),
         plotType=c("persp","contour","none"),theta=-30,phi=40,...) {
#
# Explore the log likelihood surface.
#
    if(length(ngrid)==1) ngrid <- rep(ngrid,2)
    av   <- seq(alim[1],alim[2],length=ngrid[1])
    bv   <- seq(blim[1],blim[2],length=ngrid[2])
    ab   <- expand.grid(alpha=av,beta=bv)
    fab  <- apply(as.matrix(ab),1,Fun,x=x,ntop=ntop,zeta=zeta)
    z    <- matrix(fab,nrow=ngrid[1])
    rslt <- list(x=av,y=bv,z=z,ab=ab,fab=fab)
    pT <- match.arg(plotType)
    switch(EXPR=pT,
        persp = {
            persp(x=av,y=bv,z,theta=theta,phi=phi,...)
            return(invisible(rslt))
        },
        contour = {
            contour(x=av,y=bv,z,...)
            return(invisible(rslt))
        },
        none = return(rslt)
    )
}
})
