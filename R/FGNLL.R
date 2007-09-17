`FGNLL` <-
function(H, z){
    if (H<0.01 || H>0.99) 
        return(-10^10)
    r<-FGNAcf(0:(length(z)-1), H)
    DLLoglikelihood(r,z)
}

