#' Tile geom.
#'
#' Rectangular geom parameterised by location of center (\code{x} and
#' \code{y}) and size (\code{width}, \code{height}).
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
#' geom_plot(geom_tile(), pp20)
#' if (require("scales")) {
#'   pp20$fill <- cscale(pp20$z, seq_gradient_pal())
#' }
#' geom_plot(geom_tile(), pp20)
#' geom_plot(geom_tile(list(width = 0.95, height = 0.95)), pp20)
geom_tile <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("tile")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required tile
aes_required.tile <- function(geom) c("x", "y", "width", "height")

#' @S3method aes_default tile
aes_default.tile <- function(geom) build_defaults(c("line", "solid"))

#' @S3method aes_icon tile
aes_icon.tile <- function(geom) {
  grid <- seq(0, 1, length = 5)
  data.frame(expand.grid(x = grid, y = grid), fill = grey(25))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_data tile
geom_data.tile <- function(geom, data) {
  data$width <- data$width %||% resolution(data$x, FALSE)
  data$height <- data$height %||% resolution(data$y, FALSE)
  data
}

#' @S3method geom_range tile
geom_range.tile <- function(geom, data) {
  x <- range(data$x - data$width / 2, data$x + data$width / 2, na.rm = TRUE)
  y <- range(data$y - data$height / 2, data$y + data$height / 2, na.rm = TRUE)
  list(x = x, y = y)
}

#' @S3method geom_premunch rect
geom_premunch.tile <- function(geom, data) {
  data$ymax <- data$y + data$height / 2
  data$ymin <- data$y - data$height / 2
  data$xmax <- data$x + data$width / 2
  data$xmin <- data$x - data$width / 2

  geom_premunch.rect(geom, data)
}

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob rect
geom_grob.tile <- function(geom, data) {
  rectGrob(data$x, data$y, 
    width = data$width, height = data$height, 
    default.units = "native", just = c("center", "center"), gp = gpar(
      col = data$colour, fill = alpha(data$fill, data$alpha), 
      lwd = data$size * .pt, lty = data$linetype, lineend = "butt")
  )
}
