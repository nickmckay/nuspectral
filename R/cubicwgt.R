#' 
"cubicwgt" <-
function(p) { a<-abs(p); ifelse(a<1,1+a*a*(2*a-3),0) }
