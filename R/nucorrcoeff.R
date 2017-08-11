#'@export
"nucorrcoeff" <-
function(X1, Y1, X2, Y2, t, o, wgt=cubicwgt, wgtrad=1)
{   so <- 0.05*o
    rx1 <- subset(X1, abs(X1-t)*so<wgtrad)
    ry1 <- subset(Y1, abs(X1-t)*so<wgtrad)
    rx2 <- subset(X2, abs(X2-t)*so<wgtrad)
    ry2 <- subset(Y2, abs(X2-t)*so<wgtrad)
    print(length(rx1))
    s = sum(wgt((rx1-t)*so)) * sum(wgt((rx2-t)*so))
    if(s!=0)
        sum(wgt((rx1-t)*so)*exp(1i*o*(rx1))*ry1) *
        sum(wgt((rx2-t)*so)*exp(1i*o*(rx2))*ry2) / s
    else
        0
}


