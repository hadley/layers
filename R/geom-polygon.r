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
  geom_from_call("polygon")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required polygon
aes_required.polygon <- function(geom) c("x", "y")

#' @S3method aes_default polygon
aes_default.polygon <- function(geom) build_defaults(c("line", "solid"))

#' @S3method aes_icon polygon
aes_icon.polygon <- function(geom) {
  data.frame(
    x = c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3), 
    y = c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3))
}

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob polygon
geom_grob.polygon <- function(geom, data) {
  data <- list_to_df(data)
  
  aes <- constant_aesthetics(data, c("x", "y", "order"))
  if (anyDuplicated(aes$group)) {
    stop("Some groups have duplicated aesthetics. Polygons must have 
      constant fill, colour, alpha, size and linetype.")
  }
  
  data <- data[order(data$group), ]
  aes <- aes[order(aes$group), ]
  
  polygonGrob(data$x, data$y, default.units = "native",
    id = as.integer(factor(data$group)), gp = gpar(
      col = aes$colour, fill = alpha(aes$fill, aes$alpha), 
      lwd = aes$size * .pt, lty = aes$linetype))
}
