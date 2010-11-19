#' Polygon, a filled path.
#'
#' @seealso \code{\link{geom_path}}: an unfilled polygon
#' @seealso \code{\link{geom_ribbon}}: a polygon anchored on the x-axis
#' @examples
#' if (require(maps)) {
#'   ni <- data.frame(map("nz", "North.Island", plot = F)[c("x", "y")])
#'   si <- data.frame(map("nz", "South.Island", plot = F)[c("x", "y")])
#'   geom_plot(geom_polygon(), ni)
#'   geom_plot(geom_polygon(list(colour = "grey50", fill = NA)), si)
#'
#'   nz <- rbind(data.frame(ni, group = 1), data.frame(si, group = 2))
#'   geom_plot(geom_polygon(), nz)
#' }
geom_polygon <- function(aesthetics = list()) {
  geom <- geom_from_call("polygon")
  check_aesthetic_params(geom, geom$aes_data)
  geom
}
geom_grob.polygon <- function(geom, data, ...) {
  data <- as.data.frame(calc_aesthetics(geom, data))

  aes <- data[intersect(names(data), 
    setdiff(all_aesthetics, c("x", "y", "order")))]
  aes <- aes[!duplicated(id(aes)), ]
  if (any(duplicated(aes$group))) {
    stop("Some groups have duplicated aesthetics. Polygons must have 
      constant fill, colour, size and linetype.")
  }    

  data <- data[order(data$group), ]
  aes <- aes[order(aes$group), ]

  polygonGrob(data$x, data$y, default.units = "native", ...,
    id = as.integer(factor(data$group)), gp = gpar(
      col = aes$colour, fill = alpha(aes$fill, aes$alpha), 
      lwd = aes$size * .pt, lty = aes$linetype))
}

aes_required.polygon <- function(geom) c("x", "y")
aes_default.polygon <- function(geom) build_defaults(c("line", "solid"))

geom_visualise.polygon <- function(geom, data = list()) {
  defaults <- list(
    x = c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3), 
    y = c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3))
  data <- modifyList(defaults, data)

  geom_grob(geom, data, default.units = "npc")
}

# Notes for polygon enhancements:
#  * ideally should be able to supply data frame that contains the 
#    aesthetic values for each group
#  * this data needs to be part of the scale training process
#  * if supplied, need to enforce the presence of an additional id aesthetic 
#    which matches the aesthetic data with the position data 