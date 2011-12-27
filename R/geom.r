#' Convenience method for plotting geoms.
#' 
#' This illustrates the basic pipeline by which the geom methods are called,
#' and makes it easier to visually test geoms, but graphics packages will 
#' usually call the methods individually themselves.
#'
#' @export
geom_plot <- function(geom, data = list(), munch = FALSE) {
  data <- add_group(data)
  data <- geom_data(geom, data)
  
  check_required_aesthetics(geom, data)
  
  if (munch) {
    munched <- geom_premunch(geom, data)
    geom <- munched$geom
    data <- munched$data
  }
  grob <- geom_draw(geom, data)
  rng <- geom_range(geom, data)

  grid.newpage()
  pushViewport(dataViewport(rng$x, rng$y))
  grid.draw(grob)
  
  invisible(grob)
}


#' Render a grid grob from a geom and a dataset.
#' 
#' This is the key method to implement when creating a new geom.  Given a
#' geom and its paramters, and a dataset, it renders the data to produce a
#' grid grob. The data supplied to this function has already been scaled,
#' so all values are interpretable by grid, and data and aesthetics have 
#' been combined into a single list by \code{\link{geom_draw}}
#'
#' @return a grob
geom_grob <- function(geom, data, ...) UseMethod("geom_grob")

#' Prepare the data for munching (if needed).
#'
#' This function will only be run if the coordinate system is non-linear, and
#' requires "munching" (breaking the data into small enough pieces that they
#' will still be linear after transformation).  This method doesn't actually
#' do the munching - it just gets the data into a standard form where it can
#' happen.
#'
#' This usually requires reparameterising the geom to by something that can
#' be drawn by \code{\link{geom_path}} or \code{\link{geom_polygon}}, and so
#' the function should return both the data and the new geom to be used to
#' draw the data. 
#' 
#' The default method leaves the data and geom unchanged.
#'
#' @return list containing \itemize{
#'   \item{data}{the munched data}
#'   \item{geom}{the new geom}
#' }
#' @export
#' @S3method geom_premunch default
geom_premunch <- function(geom, data) UseMethod("geom_premunch")
geom_premunch.default <- function(geom, data) list(geom = geom, data = data)

#' Process data for the geom.
#'
#' This method is run just prior to creating the grob, and is used to get the
#' data into a format which requires minimal processing to be supplied to a
#' geom. This is separated out into a separate method because a number of 
#' grobs process data in a slightly different way but otherwise inherit all
#' other behaviours, and to make testing easier. 
#'
#' This is done at the last minute to avoid passing around large chunks of
#' duplicated data
#' 
#' The default behaviour uses \code{\link{calc_aesthetics}} to update the
#' data with the aesthetic parameters and defaults stored in the geom.  All
#' grob methods should probably eventually call this default behaviour.
#' 
#' @export
#' @S3method geom_data default
#' @return a list, suitable for operation with \code{\link{geom_data}}
geom_data <- function(geom, data) UseMethod("geom_data")
geom_data.default <- function(geom, data) {
  calc_aesthetics(geom, data)  
}

#' An iconic version of the geom.
#' 
#' Suitable for use in GUIs, documentation etc.
#'
#' @export
#' @S3method geom_visualise default
geom_visualise <- function(geom, data) UseMethod("geom_visualise")
geom_visualise.default <- function(geom, data = list()) {
  geom_grob(geom, data, default.units = "npc")
}

geom_visualise <- function(geom, data = list()) {
  data <- modifyList(aes_icon(geom), data)
  geom_plot(geom, as.data.frame(data, stringsAsFactors = FALSE))
}


#' The name of the geom.
#'
#'
#' @export
geom_name <- function(geom) {
  paste("geom_", class(geom)[1], sep = "")
}


#' Compute the range of the data supplied to a geom.
#'
#' The default method inspects x and y aesthetics and data values. This 
#' function should be called with data passed through \code{\link{geom_data}}
#' (and possibly through \code{\link{geom_premunch}}) so only the data component
#' generally needs to be inspected.
geom_range <- function(geom, data) {
  UseMethod("geom_range")
}
#' @S3method geom_range default
geom_range.default <- function(geom, data) {
  x <- range(data$x, na.rm = TRUE)
  y <- range(data$y, na.rm = TRUE)
  list(x = x, y = y)
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
  values <- unlist(lapply(geom, deparse, control = NULL))
  args <- paste(names(geom), " = ", values, collapse = ", ", sep = "")
  
  paste(geom_name(geom), "(", args, ")", sep = "")
}

geom_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  geom <- structure(arguments, class = c(name, "geom"))
  check_aesthetic_params(geom, geom$aesthetics)
  geom
}

print.geom <- function(x, ...) {
  cat(geom_deparse(x), "\n")
}

