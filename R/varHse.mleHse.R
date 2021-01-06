varHse.mleHse <- function(ao,...) {
    alpha  <- ao["alpha"]
    beta   <- ao["beta"]
    ntop   <- attr(ao,"ntop")
    zeta   <- attr(ao,"zeta")
    nbot <- 0+!zeta
    x    <- nbot:ntop
    mu   <- expValHse(alpha,beta,ntop,zeta)
    sum((x-mu)^2 * dhse(x,alpha,beta,ntop,zeta))
}
