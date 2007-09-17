`predict.FitFGN` <-
function(object, n.ahead = 1, ...){
    z<-object$z
    n<-length(z)
    H<-object$H
    zm<-object$muHat
    r<-var(z)*FGNAcf(0:(n+n.ahead-1), H)
    TrenchForecast(z,r,zm,n,maxLead=n.ahead)
}

