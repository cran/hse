meHse <- function (x,ntop) {
# Moment estimation (rough!) for the hse distribution.
    xbar <- mean(x,na.rm=TRUE)
    s2   <- var(x,na.rm=TRUE)
    n    <- ntop
    a    <- (n+1-xbar)/(1+xbar)
    alpha <- a*(n+2)^2/(s2*(a+1)^3) - 1/(a+1)
    beta  <- a*alpha
    c(alpha=alpha,beta=beta)
}
