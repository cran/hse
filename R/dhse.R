dhse <- function(i,alpha,beta,ntop,zeta=FALSE) {
   nbot <- 0+!zeta
   if(!all(i%in%c(nbot:ntop,NA))) {
       whinge <- paste0("Entries of \"i\" must be integers between ",
                        nbot," and ",ntop," or NA.\n")
       stop(whinge)
   }
   len <- ntop+2+zeta
   x   <- seq(0,1,length=len)[-c(1,len)]
   p   <- dbeta(x,alpha,beta)
   (p/sum(p))[i+zeta]
}
