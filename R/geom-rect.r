#' 2d rectangles.
#' 
#' @examples
#' df <- data.frame(
#'   x = sample(10, 20, replace = TRUE),
#'   y = sample(10, 20, replace = TRUE)
#' )
#' ggplot(df, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 2)) +
#'   geom_rect()
geom_rect <- function(...) {
  new_layer(..., geom = geom_from_layer("rect"))
}

aes_required.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")
aes_present.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")
aes_default.rect <- function(geom) {
  list(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = 1)
}

geom_grob.rect <- function(geom, data) {
  data <- calc_aesthetics(geom, data)
  
  with(data, rectGrob(
    xmin, ymax, width = xmax - xmin, height = ymax - ymin, 
    default.units = "native", just = c("left", "top"), 
    gp = gpar(
      col = colour, fill = alpha(fill, alpha), 
      lwd = size * .pt, lty = linetype, lineend = "butt"
    )))
}

#' Convert rectangles into polygons with four corners
geom_munch.rect <- function(geom, data) {
  rect_to_poly <- function(xmin, xmax, ymin, ymax) {
    data.frame(
      y = c(ymax, ymax, ymin, ymin, ymax),
      x = c(xmin, xmax, xmax, xmin, xmin)
    )
  }
  
  aesthetics <- setdiff(
    names(data), c("x", "y", "xmin","xmax", "ymin", "ymax")
  )
  polys <- adply(data, 1, function(row) {
    poly <- with(row, rect_to_poly(xmin, xmax, ymin, ymax))
    aes <- as.data.frame(row[aesthetics], 
      stringsAsFactors = FALSE)[rep(1,5), ]
  })
  
  list(geom = new_geom("poly"), data = data)  
}

  
geom_visualize.rect <- function(geom, data = list()) {
  defaults <- list(
    xmin = c(0.2, 0.6),
    xmin = c(0.4, 0.8),
    ymin = c(0.2, 0.4),
    ymax = c(0.6, 1)
  )
  data <- modifyList(defaults, data)
  geom_grob(geom, data, default.units = "npc")
}
