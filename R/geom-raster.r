#' Raster geom.
#'
#' High performance rectangular geom parameterised by location of center
#' (\code{x} and \code{y}).
#'
#' @family rectangular geoms
#' @export
#' @examples
#' pp <- function (n,r=4) {
#'  x <- seq(-r*pi, r*pi, len=n)
#'  df <- expand.grid(x=x, y=x)
#'  df$r <- sqrt(df$x^2 + df$y^2)
#'  df$z <- cos(df$r^2)*exp(-df$r/6)
#'  df
#' }                                 
#' pp20 <- pp(20)
#' geom_plot(geom_raster(), pp20)
#' if (require("scales")) {
#'   pp20$fill <- cscale(pp20$z, seq_gradient_pal())
#' }
#' geom_plot(geom_raster(), pp20)
geom_raster <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("raster")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required raster
aes_required.raster <- function(geom) c("x", "y")

#' @S3method aes_default raster
aes_default.raster <- function(geom) build_defaults(c("line", "solid"))

#' @S3method aes_icon raster
aes_icon.raster <- function(geom) {
  grid <- seq(0, 1, length = 5)
  data.frame(expand.grid(x = grid, y = grid), fill = grey(25))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_range raster
geom_range.raster <- function(geom, data) {
  raster_range(data)
}

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob rect
geom_grob.raster <- function(geom, data) {
  raster <- acast(data, list("x", "y"), value.var = "fill")
  rng <- raster_range(data)
  
  rasterGrob(raster, rng$x[1], rng$y[1], diff(rng$x), diff(rng$y),
    default.units = "native", just = c("left", "bottom"), interpolate = FALSE)
}

raster_range <- function(data) {
  width <- resolution(data$x)
  height <- resolution(data$y)

  x_rng <- range(data$x, na.rm = TRUE)
  y_rng <- range(data$y, na.rm = TRUE)
  
  list(
    x = x_rng + c(-1, 1) * width / 2, 
    y = y_rng + c(-1, 1) * height / 2
  )
}
