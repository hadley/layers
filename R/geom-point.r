#' @param na.rm If \code{TRUE} silently remove missing values, otherwise
#'   warn that they are being removed.
#' @export
#' @examples
#' df <- data.frame(x = seq(0,1, 0.1), y = seq(0, 1, 0.1))
#' geom_plot(geom_point(), df)
#' geom_plot(geom_point(list(colour = "red")), df)
#' geom_plot(geom_point(list(size = 3, shape = 15)), df)
#'
#' geom_plot(geom_point(list(x = 1:10, y = 10:1)))
geom_point <- function(aesthetics = list(), na.rm = FALSE, ...) {
  geom_from_call("point")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required path
aes_required.point <- function(geom) c("x", "y")

#' @S3method aes_present path
aes_present.point <- function(geom) c("size", "shape")

#' @S3method aes_default path
aes_default.point <- function(geom) build_defaults("point")

#' @S3method geom_visualise path
aes_icon.point <- function(geom) {
  pos <- seq(0.1, 0.9, length = 6)
  data.frame(x = pos, y = pos, size = 1, shape = 19)
}

# Drawing --------------------------------------------------------------------

#' @S3method geom_grob path
geom_grob.point <- function(geom, data) {
  gp <- gpar(col = alpha(data$colour, data$alpha), fill = data$fill, 
    fontsize = data$size * .pt)
  pointsGrob(data$x, data$y, size = unit(data$size, "mm"), pch = data$shape,
    gp = data$gp)    
}

