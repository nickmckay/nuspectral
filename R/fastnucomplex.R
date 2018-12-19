#' @useDynLib nuspectral fastnucomplex
"fastnucomplex" <-
function(X, Y, omegamax, ncoeff, noctave)
 .C("fastnucomplex",
    as.double(X),
    as.complex(Y),
    as.integer(min(length(X),length(Y))),
    as.double(X[[length(X)]]-X[[1]]),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(omegamax),
    rp = complex(noctave*ncoeff))$rp
