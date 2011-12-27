#' Connect observations with stairs.
#'
#' Equivalent to plot(type='s').
#'
#' @param direction = "direction of stairs: 'vh' for vertical then horizontal,
#'    or 'hv' for horizontal then vertical"
#' @family line geoms
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = 1:10)
#' geom_plot(geom_step(), df)
#' geom_plot(geom_step(direction = "vh"), df)
geom_step <- function(aesthetics = list(), direction = "hv") {
  geom_from_call(c("step", "path"))
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_icon step
aes_icon.step <- function(geom) {
  data.frame(x = seq(0, 1, length = 15), y = seq(0, 1, length = 15))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_data step
geom_data.step <- function(geom, data, ...) {
  stairstep(data, geom$direction)
}

stairstep <- function(data, direction="hv") {
  direction <- match.arg(direction, c("hv", "vh"))
  data <- list_to_df(data)[order(data$x), ]
  n <- nrow(data)
  
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each=2))
  } else {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each=2))
  }
  
  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y")), drop = FALSE]
  ) 
}
