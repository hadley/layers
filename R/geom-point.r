geom_point <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("point")
}

aes_required.point <- function(geom) c("x", "y")
aes_present.point <- function(geom) c("x", "y", "size", "shape")
aes_default.point <- function(geom) build_defaults("point")

geom_grob.point <- function(geom, data, ...) {
  data <- calc_aesthetics(geom, data)

  with(data, {
    gp <- gpar(col = alpha(colour, alpha), fill = fill, fontsize = size * .pt)
    pointsGrob(x, y, size = unit(size, "mm"), pch = shape, gp = gp, ...)    
  })
}

geom_visualize.point <- function(geom, data = list()) {
  pos <- seq(0.1, 0.9, length = 6)
  defaults <- list(x = pos, y = pos, size = 0.5, shape = 19)
  data <- modifyList(defaults, data)

  geom_grob(geom, data, default.units = "npc")
}

