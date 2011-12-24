#' Count number of observation in rectangular bins.
#' 
#' @name stat_bin2d
#' @seealso \code{\link{stat_binhex}} for hexagonal binning
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, price))
#' d + stat_bin2d()
#' d + geom_bin2d()
#' 
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + stat_bin2d(bins = 10)
#' d + stat_bin2d(bins = 30)
#' 
#' # Or by specifying the width of the bins
#' d + stat_bin2d(binwidth = c(1, 1000))
#' d + stat_bin2d(binwidth = c(.1, 500))
#' 
#' # Or with a list of breaks
#' x <- seq(min(diamonds$carat), max(diamonds$carat), by = 0.1)
#' y <- seq(min(diamonds$price), max(diamonds$price), length = 50)
#' d + stat_bin2d(breaks = list(x = x, y = y))
#' 
#' # With qplot
#' qplot(x, y, data = diamonds, geom="bin2d", 
#'   xlim = c(4, 10), ylim = c(4, 10))
#' qplot(x, y, data = diamonds, geom="bin2d", binwidth = c(0.1, 0.1),
#'   xlim = c(4, 10), ylim = c(4, 10))
StatBin2d <- proto(Stat, {
  objname <- "bin2d"

  default_aes <- function(.) aes(fill = ..count..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomRect

  calculate <- function(., data, scales, binwidth = NULL, bins = c(30, 30), breaks = NULL, origin = NULL, drop = TRUE, ...) {
    
    bins <- bin_rect(data$x, data$y, data$weight, 
      xbreaks = interval_breaks(bins[1], binwidth[1], 
        range = scales$x$output_set()),
      ybreaks = interval_breaks(bins[2], binwidth[2], 
        range = scales$y$output_set()),
    )
    
    if (drop) bins <- bins[bins$count > 0, ]
    bins$density <- bin$count / sum(bin$count, na.rm = TRUE)
    bins
  }
})
