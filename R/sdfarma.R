sdfarma <- function(n, phi=numeric(0), theta=numeric(0)) {
    lams <- 2*pi*seq(from=1/n, to=1/2, by=1/n)
    nf <- length(lams)
    a <- outer(lams, 1:length(theta))
    if (length(theta)>0) {
            C <- cbind(1, cos(a))%*% c(1, -theta)
            S <- sin(a) %*% theta
        } else {
            C <- 1
            S <- 0
        }
    num <- as.vector(C*C + S*S)/(2*pi)
        if (length(phi)>0) {
            C <- cbind(1, cos(a))%*% c(1, -phi)
            S <- sin(a) %*% theta
        } else {
            C <- 1
            S <- 0
        }
    den <- as.vector(C*C+S*S)
    num/den
    }
