#' 2d rectangles.
#' 
geom_rect <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("rect")
}


aes_required.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")
aes_present.rect <- function(geom) c("xmin", "xmax", "ymin", "ymax")
aes_default.rect <- function(geom) build_defaults(c("line", "solid"))

geom_grob.rect <- function(geom, data) {
  data <- calc_aesthetics(geom, data)
  
  rectGrob(data$xmin, data$ymax, 
    width = data$xmax - data$xmin, height = data$ymax - data$ymin, 
    default.units = "native", just = c("left", "top"), gp = gpar(
      col = data$colour, fill = alpha(data$fill, data$alpha), 
      lwd = data$size * .pt, lty = data$linetype, lineend = "butt")
  )
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
    poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
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
