#' Bars, rectangles with bases on x-axis.
#'
#' @seealso \code{\link{stat_bin}: for more details of the binning algorithm #' @seealso \code{\link{position_dodge}}: for side-by-side barcharts
#' @seealso \code{\link{position_stack}}: for more info on stacking
#'
#' @export
#' @examples
#' df <- data.frame(x = seq(0,1, 0.1), y = seq(0, 1, 0.1))
#' geom_plot(geom_bar(), df)
#' geom_plot(geom_bar(), df, munch = TRUE)
#' geom_plot(geom_bar(width = 0.1), df)
#' geom_plot(geom_bar(width = 0.05), df)
#' geom_plot(geom_bar(list(colour = "red")), df)
geom_bar <- function(aesthetics = list(), width = NULL, na.rm = FALSE, ...) {
  geom_from_call("bar")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required bar
aes_required.bar <- function(geom) c("x", "y", "width")

#' @S3method aes_default bar
aes_default.bar <- function(geom) build_defaults(c("line", "solid"))

#' @S3method aes_icon bar
aes_icon.bar <- function(geom) {
  list(x = c(0.3, 0.7), width = 0.3, y = c(0.4, 0.8))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_data bar
geom_data.bar <- function(geom, data) {
  if (is.null(data$width)) {
    data$width <- geom$width %||% (resolution(data$x, FALSE) * 0.9)
  }
  NextMethod()
}

#' @S3method geom_munch bar
geom_munch.bar <- function(geom, data) {
  geom <- geom_rect(geom$aesthetics)
  geom_munch(geom, bar_to_rect(geom, data))
}

bar_to_rect <- function(geom, data) {
  # Parameter overrides all. Calculated from data as fall back in case data
  # hasn't been aggregated by statistic that computes width.
  width <- geom$width %||% data$width %||% (resolution(data$x, FALSE) * 0.9)
  data$width <- NULL
  
  data$ymin <- pmin(data$y, 0)
  data$ymax <- pmax(data$y, 0)
  data$xmin <- data$x - width / 2
  data$xmax <- data$x + width / 2
  data$x <- NULL
  data$y <- NULL  

  data
}

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob bar
geom_grob.bar <- function(geom, data) {
  rectGrob(data$x, 0, 
    width = data$width, height = data$y, 
    default.units = "native", just = c("center", "top"), gp = gpar(
      col = data$colour, fill = alpha(data$fill, data$alpha), 
      lwd = data$size * .pt, lty = data$linetype, lineend = "butt")
  )
}

