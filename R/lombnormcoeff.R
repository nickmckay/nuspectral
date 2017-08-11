#'@export
"lombnormcoeff" <-
function(X,Y,o)
{  tau <- atan2(sum(sin(2*o*X)), sum(cos(2*o*X))) / 2

   (sum(Y*cos(o*X-tau))^2/sum(cos(o*X-tau)^2)+
    sum(Y*sin(o*X-tau))^2/sum(sin(o*X-tau)^2))  / (2*var(Y))
}
