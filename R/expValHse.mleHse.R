expValHse.mleHse <- function(ao,...) {
   alpha  <- ao["alpha"]
   beta   <- ao["beta"]
   ntop   <- attr(ao,"ntop")
   zeta   <- attr(ao,"zeta")
   expValHse.default(alpha,beta,ntop,zeta) 
}
