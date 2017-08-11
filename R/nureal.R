#'@export
"nureal" <-
function(X, Y, omegamax, ncoeff, noctave)
 .C("nureal",
    as.double(X),
    as.double(Y),
    as.integer(min(length(X),length(Y))),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(omegamax),
    rp = complex(noctave*ncoeff))$rp

