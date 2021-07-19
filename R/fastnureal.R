#' @export
fastnureal <-
function(X, Y, omegamax, ncoeff, noctave)
 .C("fastnureal",
    as.double(X),
    as.double(Y),
    as.integer(min(length(X),length(Y))),
    as.double(X[[length(X)]]-X[[1]]),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(omegamax),
    rp = complex(noctave*ncoeff))$rp
