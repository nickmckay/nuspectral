#' 
"wrap" <-
function(x, y, my.fun, ...) 
    sapply(seq(along=x), function(i) my.fun(x[[i]], y[[i]], ...))
