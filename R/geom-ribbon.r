#' Ribbons, y range with continuous x values
#'
#' @seealso \code{\link{geom_bar}}: discrete intervals (bars)
#' @seealso \code{\link{geom_linerange}}: discrete intervals (lines)
#' @seealso \code{\link{geom_polygon}}: general polygons
#' @export
#' @S3method aes_default ribbon
#' @S3method aes_required ribbon
#' @S3method geom_grob ribbon
#' @S3method geom_visualise ribbon
#' @examples
#' height <- runif(10)
#' df <- data.frame(x = 1:10, ymax = 5 + height, ymin = 5 - height)
#' geom_plot(geom_ribbon(), df)
#' geom_plot(geom_ribbon(list(colour = "red")), df)
#' geom_plot(geom_ribbon(list(colour = "red", fill = NA)), df)
geom_ribbon <- function(aesthetics = list(), ...) {
  geom_from_call("ribbon")
}
aes_required.ribbon <- function(geom) c("x", "ymin", "ymax")
aes_default.ribbon <- function(geom) build_defaults(c("solid", "line"))

geom_data.ribbon <- function(geom, data) {
  data <- as.data.frame(calc_aesthetics(geom, data), stringsAsFactors = FALSE)
  data[order(data$group, data$x), ]
}

geom_grob.ribbon <- function(geom, data) {
  aes <- constant_aesthetics(data, c("x", "ymin", "ymax", "order"))
  if (anyDuplicated(aes$group)) {
    stop("Some groups have duplicated aesthetics. Ribbons must have 
      constant fill, colour, alpha, size and linetype.")
  }

  # Instead of removing NA values from the data and plotting a single
  # polygon, we want to "stop" plotting the polygon whenever we're
  # missing values and "start" a new polygon as soon as we have new
  # values.  We do this by creating an id vector for polygonGrob that
  # has distinct polygon numbers for sequences of non-NA values and NA
  # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
  # 4, 4, 4, NA)
  missing_pos <- !complete.cases(data[c("x", "ymin", "ymax")])
  ids <- id(list(cumsum(missing_pos) + 1, data$group))
  ids[missing_pos] <- NA

  pos <- data.frame(
    x = c(data$x, rev(data$x)), 
    y = c(data$ymax, rev(data$ymin)), 
    id = c(ids, rev(ids)))

  polygonGrob(pos$x, pos$y, id = pos$id, default.units = "native",
    gp = gpar(fill = alpha(aes$fill, aes$alpha), col = aes$colour, 
      lwd = aes$size * .pt, lty = aes$linetype))
}

aes_icon.ribbon <- function(geom) {
  data.frame(
    x = c(0, 0.3, 0.5, 0.8, 1), 
    ymin = c(0.5, 0.3, 0.4, 0.2, 0.3),
    ymax = c(0.7, 0.5, 0.6, 0.5, 0.7))
}
