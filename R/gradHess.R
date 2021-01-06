grad <- local({

   h <- function(x,n,zeta){
       y <- (x+zeta)/(n+1+zeta)
       1/(y*(1-y))
   }

   T1 <- function(x,n,zeta) {
       log((x+zeta)/(n+1+zeta))
   }

   T2 <- function(x,n,zeta) {
       log(1-(x+zeta)/(n+1+zeta))
   }

function(x,alpha,beta,ntop,zeta) {
   x <- x[!is.na(x)]
   nbot <- 0+!zeta
   iv <- nbot:ntop
   av <- h(iv,ntop,zeta)
   bv <- T1(iv,ntop,zeta)
   cv <- T2(iv,ntop,zeta)
   E  <- sum(av*exp(alpha*bv + beta*cv))
   dEdAlpha <- sum(av*bv*exp(alpha*bv + beta*cv))
   dEdBeta  <- sum(av*cv*exp(alpha*bv + beta*cv))
   dAdAlpha <- dEdAlpha/E
   dAdBeta  <- dEdBeta/E
   c(sum(T1(x,ntop,zeta)-dAdAlpha),sum(T2(x,ntop,zeta)-dAdBeta))
}
})

hess <- local({
function(alpha,beta,ntop,zeta,ndata) {
   nbot <- 0+!zeta
   iv <- nbot:ntop
   av <- h(iv,ntop,zeta)
   bv <- T1(iv,ntop,zeta)
   cv <- T2(iv,ntop,zeta)
   E  <- sum(av*exp(alpha*bv + beta*cv))

   dEdAlpha <- sum(av*bv*exp(alpha*bv + beta*cv))
   dEdBeta  <- sum(av*cv*exp(alpha*bv + beta*cv))

   d2EdAlpha2 <- sum(av*bv^2*exp(alpha*bv + beta*cv))
   d2EdBeta2  <- sum(av*cv^2*exp(alpha*bv + beta*cv))
   d2EdAlphaDbeta <- sum(av*bv*cv*exp(alpha*bv + beta*cv)) 

   d2AdAlpha2 <- (d2EdAlpha2 - dEdAlpha^2/E)/E
   d2AdBeta2  <- (d2EdBeta2 - dEdBeta^2/E)/E
   d2AdAlphaDbeta <- (d2EdAlphaDbeta - dEdAlpha*dEdBeta/E)/E
   -ndata*matrix(c(d2AdAlpha2,d2AdAlphaDbeta,d2AdAlphaDbeta,d2AdBeta2),nrow=2)
}
},envir=environment(grad))
