#' Compute the density at each point in the data
#'
#' Requires the locfit package.
#' 
#' @TODO explore if this approach is also fast enough for 1d and 2d density 
#'   estimates
#' @param scale should x and y positions be scaled independently?
#' @seealso \code{\link{locfit}} for the algorithm used to calculate the
#'  density
stat_densitypoint <- function(scale = TRUE) {
  stopifnot(require("locfit"))
  stat_from_call("densitypoint")
}

#' data(diamonds, package = "ggplot2")
#' df <- diamonds[c("price", "carat")]
#' names(df) <- c("x", "y")
#' stat_transform(stat_densitypoint(), df)
stat_transform.densitypoint <- function(stat, data, xrange, yrange) {
  mod <- locfit( ~ lp(x, y, scale = stat$scale), data = data)
  
  out <- unique(data[c("x", "y")])
  out$density <- predict(mod, out)
  out
}