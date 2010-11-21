#' Connect observations with stairs.
#'
#' Equivalent to plot(type='s').
#'
#' @param direction = "direction of stairs: 'vh' for vertical then horizontal,
#'    or 'hv' for horizontal then vertical"
#' @export
#' @S3method geom_data step
#' @S3method geom_visualise step
#' df <- data.frame(x = 1:10, y = 1:10)
#' geom_plot(geom_step(), df)
#' geom_plot(geom_step(direction = "vh"), df)
geom_step <- function(aesthetics = list(), direction = "hv") {
  geom_from_call(c("step", "path"))
}

geom_data.step <- function(geom, data, ...) {
  data <- as.data.frame(calc_aesthetics(geom, data), stringsAsFactors = FALSE)
  stairstep(data, geom$direction)
}

aes_icon.step <- function(geom) {
  data.frame(x = 1:15, y = 1:15)
}

stairstep <- function(data, direction="hv") {
  direction <- match.arg(direction, c("hv", "vh"))
  data <- as.data.frame(data)[order(data$x), ]
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
