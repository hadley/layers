#' 2d rectangles.
#' 
#' @export
geom_rect <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("rect")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required rect
aes_required.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")

#' @S3method aes_default rect
aes_default.rect <- function(geom) build_defaults(c("line", "solid"))

#' @S3method aes_icon rect
aes_icon.rect <- function(geom) {
  data.frame(
    xmin = c(0.1, 0.6),
    xmax = c(0.4, 0.8),
    ymin = c(0.2, 0.1),
    ymax = c(0.6, 0.9)
  )
}


#' @S3method geom_range rect
geom_range.rect <- function(geom, data) {
  x <- range(data$xmin, data$xmax, na.rm = TRUE)
  y <- range(data$ymin, data$ymax, na.rm = TRUE)
  list(x = x, y = y)
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_premunch rect
geom_premunch.rect <- function(geom, data) {
  data <- list_to_df(data)
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

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob rect
geom_grob.rect <- function(geom, data) {
  rectGrob(data$xmin, data$ymax, 
    width = data$xmax - data$xmin, height = data$ymax - data$ymin, 
    default.units = "native", just = c("left", "top"), gp = gpar(
      col = data$colour, fill = alpha(data$fill, data$alpha), 
      lwd = data$size * .pt, lty = data$linetype, lineend = "butt")
  )
}

