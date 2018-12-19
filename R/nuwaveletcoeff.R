#'@export
"nuwaveletcoeff" <-
function(X, Y, t, o, wgt=cubicwgt, wgtrad=1, sigma = 0.05)
{   so <- sigma*o
    rx <- subset(X, abs(X-t)*so<wgtrad)
    ry <- subset(Y, abs(X-t)*so<wgtrad)
    s <- sum(wgt((X-t)*so))
    if(s!=0)
        sum(wgt((rx-t)*so)*exp(1i*o*(rx-t))*ry)/s
    else
        0
}
