#' Connect observations, in ordered by x value
#' 
#' @seealso \code{link{geom_path}}: free path, not ordered by x
#' @seealso \code{link{geom_segment}}: line segments
#' @seealso \code{link{geom_ribbon}}: fill between line and x-axis
#' @export geom_line
#' @examples
#' df <- data.frame(x = sample(1:10), y = sample(1:10))
#' geom_plot(geom_line(), df)
#' geom_plot(geom_line(list(colour = "red")), df)
#' geom_plot(geom_line(list(size = 3)), df)
geom_line <- function(aesthetics = list(), arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  geom_from_call(c("line", "path"))
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_icon line
aes_icon.line <- function(geom) {
  data.frame(
    x = c(0, 0.25, 0.5, 0.75, 1),
    y = c(0.2, 0.7, 0.4, 0.8, 0.3))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_data line
geom_data.line <- function(geom, data, ...) {
  data <- as.data.frame(NextMethod(), stringsAsFactors = FALSE)
  data[order(data$group, data$x), ]
}

