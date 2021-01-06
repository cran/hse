mcCovMat <- function(object,nsim=100,seed=NULL) {
if(!inherits(object,"mleHse"))
    stop("Argument \"object\" must be of class \"mleHse\".\n")
if(is.null(seed)) seed <- sample(1:1e5,1)
set.seed(seed)
simdat <- simulate(object,nsim=nsim)
fitz   <- lapply(simdat,mleHse,ntop=attr(object,"ntop"),
                 zeta=attr(object,"zeta"),par0=as.vector(object))
simpar <- matrix(unlist(fitz),byrow=TRUE,ncol=2)
xbar   <- apply(simpar,2,mean)
rslt   <- var(simpar)
rslt   <- (nsim-1)*rslt/nsim + (xbar-object)%*%t(xbar-object)
rownames(rslt) <- colnames(rslt) <- c("alpha","beta")
attr(rslt,"seed") <- seed
rslt
}
