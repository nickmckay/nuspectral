#' @useDynLib nuspectral nucomplex
"nucomplex" <-
function(X, Y, omegamax, ncoeff, noctave)
 .C("nucomplex",
    as.double(X),
    as.complex(Y),
    as.integer(min(length(X),length(Y))),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(omegamax),
    rp = complex(noctave*ncoeff))$rp
