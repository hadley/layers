#' Single line segments.
#' 
#' @param arrow specification for arrow heads, as created by arrow()
#' @seealso \code{\link{geom_path}} connect multiple points
#' @seealso \code{\link{geom_line}} connect points ordered along x.
#' @export
#' @S3method aes_required segment
#' @S3method aes_default segment
#' @S3method geom_grob segment
#' @S3method geom_munch segment
#' @S3method geom_visualise segment
#' @examples
#' df <- data.frame(x = runif(20), y = runif(20))
#' df <- transform(df, 
#'   xend = x + rnorm(20, sd = 1/5), 
#'   yend = y + rnorm(20, sd = 1/5))
#' geom_plot(geom_segment(), df)
#' geom_plot(geom_segment(), df, munch = TRUE)
#' geom_plot(geom_segment(arrow = arrow()), df)
geom_segment <- function(aesthetics = list(), arrow = NULL) {
  geom_from_call("segment")
}

aes_required.segment <- function(geom) c("x", "y", "xend", "yend")
aes_default.segment <- function(geom) build_defaults("line")

geom_grob.segment <- function(geom, data, ...) {
  segmentsGrob(data$x, data$y, data$xend, data$yend, default.units = "native",
    gp = gpar(col = alpha(data$colour, data$alpha), lwd = data$size * .pt, 
      lty = data$linetype, lineend = "butt"), 
    arrow = geom$arrow)
}

geom_munch.segment <- function(geom, data) {
  data <- as.data.frame(data)
  data$group <- 1:nrow(data)

  starts <- data[, setdiff(names(data), c("xend", "yend"))]

  ends <- data[, setdiff(names(data), c("x", "y"))]
  ends$x <- ends$xend
  ends$y <- ends$yend
  ends$xend <- NULL
  ends$yend <- NULL
  
  list(geom = geom_path(geom$aesthetics), data = rbind(starts, ends))
}

aes_icon.segment <- function(geom) {
  data.frame(
    x = c(0.1, 0.3, 0.5, 0.7),
    y = c(0.3, 0.5, 0.1, 0.9), 
    xend = c(0.2, 0.5, 0.7, 0.9),
    yend = c(0.8, 0.7, 0.4, 0.3))
}

