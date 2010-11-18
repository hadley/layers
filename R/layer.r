#' The layers package.
#' 
#' @imports
#' @doctype package
#' @name layers
#' @aliases layers package-layers
NULL

layer_path <- function(arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  new_layer(..., geom = geom_from_layer("path"))
}

layer_point <- function(..., na.rm = FALSE) {
  new_layer(..., geom = geom_from_layer("point"))
}

layer_rect <- function(...) {
  new_layer(..., geom = geom_from_layer("rect"))
}

layer_bar <- function(width = NULL, na.rm = FALSE, ...) {
  new_layer(..., geom = geom_from_layer("bar"))
}


geom_from_layer <- function(name, call = NULL) {
  if (is.null(call)) {
    call <- match.call(sys.call(sys.parent(2)), expand.dots = FALSE)
  }
  
  aes <- call$`...`[aes_all(structure(class = name))]
    
  call$`...` <- NULL
  call$aesthetics <- do.call("call", list(as.name(list), aes))
  
  geom_from_call(name, call)
}