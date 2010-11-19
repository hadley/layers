#' The layers package.
#' 
#' @imports scales plyr
#' @doctype package
#' @name layers
#' @aliases layers package-layers
NULL

geom_from_layer <- function(name, call = NULL) {
  if (is.null(call)) {
    call <- match.call(sys.call(sys.parent(2)), expand.dots = FALSE)
  }
  
  aes <- call$`...`[aes_all(structure(class = name))]
    
  call$`...` <- NULL
  call$aesthetics <- do.call("call", list(as.name(list), aes))
  
  geom_from_call(name, call)
}