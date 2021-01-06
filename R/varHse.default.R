varHse.default <- function(ao,beta,ntop,zeta=FALSE,...) {
    nbot <- 0+!zeta
    x    <- nbot:ntop
# Note that "ao" is really "alpha"!
    mu   <- expValHse(ao,beta,ntop,zeta)
    sum((x-mu)^2 * dhse(x,ao,beta,ntop,zeta))
}
