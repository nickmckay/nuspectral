#'@export
"nurealcoeff" <-
function(X,Y,o)
{   if(o!=1)
    {   n     <- length(X)
        zeta  <- sum(Y*exp(-1i*o*X))
        iota2 <- sum(exp(-2i*o*X))
        2*(n*Conj(zeta)-Conj(iota2)*zeta)/(n*n-Conj(iota2)*iota2)
    }
    else
    {   mean(Y)   }
}
