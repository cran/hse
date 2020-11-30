varHse <- function(alpha,beta,ntop,zeta=FALSE) {
    nbot <- 0+!zeta
    x    <- nbot:ntop
    mu   <- expValHse(alpha,beta,ntop,zeta)
    sum((x-mu)^2 * dhse(x,alpha,beta,ntop,zeta))
}
