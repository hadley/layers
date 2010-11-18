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
