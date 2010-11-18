#' Bars, rectangles with bases on x-axis.
#'
#' @seealso \code{\link{stat_bin}: for more details of the binning algorithm #' @seealso \code{\link{position_dodge}}: for side-by-side barcharts
#' @seealso \code{\link{position_stack}}: for more info on stacking
geom_bar <- function(aesthetics = list(), width = NULL, na.rm = FALSE, ...) {
  geom_from_call(c("bar", "rect"))
}

geom_stat.bar <- function(geom, ...) stat_bin(...)
geom_adjust.bar <- function(geom, ...) adjust_stack(...)

geom_reparam.bar <- function(geom, data) {
  data$width <- data$width %||% 
    geom$width %||% (resolution(data$x, FALSE) * 0.9)
  transform(data,
    ymin = pmin(y, 0), ymax = pmax(y, 0),
    xmin = x - width / 2, xmax = x + width / 2, width = NULL
  )
}

geom_visualize.bar <- function(geom, data = list()) {
  pos <- list(
    x = c(0.3, 0.7),
    width = 0.3,
    y = c(0.4, 0.8)
  )
  data <- modifyList(pos, data)
  geom_grob(geom, data, default.units = "npc")
}
