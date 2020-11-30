expValHse <- function(alpha,beta,ntop,zeta=FALSE) {
    nbot <- 0+!zeta
    x <- nbot:ntop
    sum(x*dhse(x,alpha,beta,ntop,zeta))
}
