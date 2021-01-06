gof <- function(x,obsd,test=TRUE,MC=FALSE,nsim=99) {
#
# Goodness of fit test for the hse distribution.
#
if(!inherits(x,"mleHse"))
    stop("Argument \"x\" must be of class \"mleHse\".\n")
ntop <- attr(x,"ntop")
zeta <- attr(x,"zeta")
nbot <- 0+!zeta
xi   <- nbot:ntop
ndat <- sum(!is.na(obsd))
E    <- dhse(xi,x[1],x[2],ntop=ntop,zeta=zeta)*ndat
O    <- table(factor(obsd,levels=xi))
stat <- sum((O-E)^2/E)
if(!test) return(stat)
if(MC) {
    simdat <- simulate(x,nsim=nsim)
    fitz   <- lapply(simdat,function(sd,ntop,zeta){mleHse(sd,
                            ntop=ntop,zeta=zeta)},ntop=ntop,zeta=zeta)
    cmpr   <- lapply(1:nsim,function(k,fitz,obsd){gof(fitz[[k]],
                            obsd[[k]],test=FALSE)},fitz=fitz,obsd=simdat)
    pval   <- (1+sum(cmpr >= stat))/(1+nsim)
    return(list(stat=stat,pval=pval))
}
esmall <- mean(E) < 5 | any(E < 1)
if(esmall) {
    whinge <- paste0("Expected values are small; chi squared test is invalid.\n",
                     "These expected values are available as the attribute",
                     " \"expVals\" of the\n","returned value.\n")
    warning(whinge)
}
dfr  <- length(E) - 3
pval <- pchisq(stat,dfr,lower.tail=FALSE)
rslt <- list(stat=stat,pval=pval,degFree=dfr)
if(esmall) attr(rslt,"expVals") <- E
rslt
}
