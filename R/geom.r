#' Render a grid grob from a geom and a dataset.
#' 
#' This is the key method to implement when creating a new geom.  Given a
#' geom and its paramters, and a dataset, it renders the data to produce a
#' grid grob. The data supplied to this function has already been scaled,
#' so all values are interpretable by grid, but it has not been fleshed
#' out with geom defaults and aesthetics parameters - use 
#' \code{\link{calc_aesthetics}} to do so.
#'
#' @return a grob
geom_grob <- function(geom, data, ...) UseMethod("geom_grob")

#' Prepare the data for munching (if needed).
#'
#' This function will only be run if the coordinate system is non-linear, and
#' requires "munching" (breaking the data into small enough pieces that they
#' will still be linear after transformation).
#'
#' This usually requires reparameterising the geom to by something that can
#' be drawn by \code{\link{geom_path}} or \code{\link{geom_polygon}}, and so
#' the function should return both the data and the new geom to be used to
#' draw the data. 
#' 
#' The default method leaves the data and geom unchanged.
#'
#' @return list containing updated data and geom that should be used to draw
#'   the data
#' @export
#' @S3method geom_premunch default
geom_munch <- function(geom, data) UseMethod("geom_munch")
geom_munch.default <- function(geom, data) list(geom = geom, data = data)

#' Default related stat and adjust.
#'
#' The defaults are the identity functions which leave the data unchanged.
#' 
#' @aliases geom_stat geom_adjust
#' @export geom_stat geom_adjust
#' @S3method geom_stat default
#' @S3method geom_adjust default
#' @usage geom_stat(geom, ...)
#' @usage geom_adjust(geom, ...)
#' @param ... Other arguments passed on to the object creation function.  
#'   These are used when initialised a geom from a name and list of 
#'   parameters, which may belong to the stat or the position adjustment.
geom_stat <- function(geom, ...) UseMethod("geom_stat")
geom_adjust <- function(geom, ...) UseMethod("geom_adjust")

geom_stat.default <- function(geom, ...) stat_identity()
geom_adjust.default <- function(geom, ...) adjust_identity()

#' @export
#' @S3method geom_visualise default
geom_visualise <- function(geom, data) UseMethod("geom_visualise")
geom_visualise.default <- function(geom, data = list()) {
  geom_grob(geom, data, default.units = "npc")
}

geom_name <- function(geom) {
  str_c("geom_", class(geom)[1])
}

#' Convenience method for plotting geoms.
#' 
#' @export
geom_plot <- function(geom, data = list(), munch = FALSE) {
  data <- add_group(data)
  data <- calc_aesthetics(geom, data)
  if (munch) {
    munched <- geom_munch(geom, data)
    geom <- munched$geom
    data <- munched$data
  }
  
  grob <- geom_draw(geom, data)

  grid.newpage()
  pushViewport(dataViewport(data$x, data$y))
  grid.draw(grob)
}

geom_draw <- function(geom, data) {
  name_grob(geom_grob(geom, data), geom_name(geom))
}
name_grob <- function(grob, name) {
  grob$name <- grobName(grob, name)
  grob
}

#' Deparse a geom into the call that created it.
#' Useful for serialising ggplot2 objects back into R code.
geom_deparse <- function(geom) {
  args <- str_c(names(geom), " = ", lapply(geom, deparse), collapse = ", ")
  str_c(geom_name(geom), "(", args, ")")
}

geom_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  geom <- structure(arguments, class = name)
  check_set_aesthetics(geom)
  geom
}
