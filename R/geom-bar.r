#' Bars, rectangles with bases on x-axis.
#'
#' @seealso \code{\link{stat_bin}: for more details of the binning algorithm #' @seealso \code{\link{position_dodge}}: for side-by-side barcharts
#' @seealso \code{\link{position_stack}}: for more info on stacking
#'
#' @export
#' @S3method aes_default bar
#' @S3method aes_required bar
#' @S3method geom_adjust bar
#' @S3method geom_munch bar
#' @S3method geom_grob bar
#' @S3method geom_stat bar
#' @S3method geom_visualise bar
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

geom_stat.bar <- function(geom, ...) stat_bin(...)
geom_adjust.bar <- function(geom, ...) adjust_stack(...)

aes_required.bar <- function(geom) c("x", "y")
aes_default.bar <- function(geom) build_defaults(c("line", "solid"))

geom_grob.bar <- function(geom, data) {
  data <- calc_aesthetics(geom, data)  
  geom_grob(geom_rect(geom$aesthetics), bar_to_rect(geom, data))
}

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

geom_visualise.bar <- function(geom, data = list()) {
  pos <- list(
    x = c(0.3, 0.7),
    width = 0.3,
    y = c(0.4, 0.8)
  )
  data <- modifyList(pos, data)
  geom_grob(geom, data, default.units = "npc")
}
