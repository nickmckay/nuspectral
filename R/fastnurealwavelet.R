#'@export
"fastnurealwavelet" <-
function(X, Y, omegamax, ncoeff, noctave, tmin, tmax, tsubdiv, sigma=0.1){
 .C("fastnurealwavelet",
    as.double(X),
    as.double(Y),
    as.integer(min(length(X),length(Y))),
    as.double(X[[length(X)]]-X[[1]]),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(tmin),
    as.double(tmax),
    as.integer(tsubdiv),
    as.double(sigma),
    as.double(omegamax),
    rp = complex(noctave*ncoeff*tsubdiv))$rp
}
# to test: fastnurealwavelet(co2[[2]],co2[[4]],0.0015,100,20,0,420000,10000) should reproduce Fig 8 from the paper
