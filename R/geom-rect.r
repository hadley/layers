#' 2d rectangles.
#' 
#' @export
#' @S3method aes_default rect
#' @S3method aes_required rect
#' @S3method geom_grob rect
#' @S3method geom_munch rect
#' @S3method geom_visualise rect
geom_rect <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("rect")
}

aes_required.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")
aes_default.rect <- function(geom) build_defaults(c("line", "solid"))

geom_grob.rect <- function(geom, data) {
  rectGrob(data$xmin, data$ymax, 
    width = data$xmax - data$xmin, height = data$ymax - data$ymin, 
    default.units = "native", just = c("left", "top"), gp = gpar(
      col = data$colour, fill = alpha(data$fill, data$alpha), 
      lwd = data$size * .pt, lty = data$linetype, lineend = "butt")
  )
}

#' Convert rectangles into polygons with four corners
geom_munch.rect <- function(geom, data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rect_to_poly <- function(xmin, xmax, ymin, ymax) {
    data.frame(
      y = c(ymax, ymax, ymin, ymin, ymax, NA),
      x = c(xmin, xmax, xmax, xmin, xmin, NA)
    )
  }
  
  pos <- as.matrix(data[c("xmin", "xmax", "ymin", "ymax")])
  locations <- adply(pos, 1, splat(rect_to_poly))

  aesthetics <- setdiff(names(data), c("xmin","xmax", "ymin", "ymax"))
  polys <- cbind(locations, data[rep(1:nrow(data), each = 6), aesthetics])
  
  list(geom = geom_polygon(geom$aesthetics), data = polys)
}

aes_icon.rect <- function(geom) {
  list(
    xmin = c(0.2, 0.6),
    xmin = c(0.4, 0.8),
    ymin = c(0.2, 0.4),
    ymax = c(0.6, 1)
  )
}
