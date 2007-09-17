`GetFitFGN` <-
function(z, MeanZeroQ=FALSE){
    y<-z
    if (!MeanZeroQ)
        y<-z-mean(z)
    EntropyH<-function(H) -FGNLL(H,y)
    out<-optimize(f=EntropyH, interval=c(0.01, 0.99))
    H<-out[[1]] #sometimes optimize incorrectly chooses the endpoint (very rare)
    if (H == 0.01 || H==0.99) {
        warning("end point reached. Switching to L-BFGS-B ...")
        H0<-HurstK(y)
        out<-optim(H0, fn=EntropyH, method="L-BFGS-B", lower=0.01, upper=0.99)
        err<-out$convergence
        if (err != 0) {
            warning(" err = ", err , ". Trying Nelder-Mead...")
            out<-optim(H0, fn=EntropyH, method="Nelder-Mead")
        }
    }
    list(loglikelihood=-out[[2]],H=out[[1]])
}
    
 
