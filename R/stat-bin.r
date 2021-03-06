#' Bin data.
#' 
#' Missing values are currently silently dropped.
#'
#' @name stat_bin
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#'   data
#' @param breaks Actual breaks to use.  Overrides bin width and origin 
#' @param origin Origin of first bin 
#' @param width Width of bars when used with categorical data 
#' @param right Should intervals be closed on the right (a, b], or not [a, b) 
#' @param drop If TRUE, remove all bins with zero counts
#' @return New data frame with additional columns:
#'   \item{count}{number of points in bin}
#'   \item{density}{density of points in bin, scaled to integrate to 1}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{ndensity}{density, scaled to maximum of 1}
#' @export
#' @examples
#' simple <- data.frame(x = rep(1:10, each = 2))
#' base <- ggplot(simple, aes(x))
#' # By default, right = TRUE, and intervals are of the form (a, b]
#' base + stat_bin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
#' # If right = FALSE intervals are of the form [a, b)
#' base + stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")
#' 
#' m <- ggplot(movies, aes(x=rating))
#' m + stat_bin()
#' m + stat_bin(binwidth=0.1)
#' m + stat_bin(breaks=seq(4,6, by=0.1))
#' # See geom_histogram for more histogram examples
#' 
#' # To create a unit area histogram, use aes(y = ..density..)
#' (linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
#'   geom="line", position="identity"))
#' linehist + stat_density(colour="blue", fill=NA)
#' 
#' # Also works with categorical variables
#' ggplot(movies, aes(x=mpaa)) + stat_bin()
#' qplot(mpaa, data=movies, stat="bin")
# StatBin <- proto(Stat, {
#   objname <- "bin"
# 
#   informed <- FALSE
#   
#   calculate_groups <- function(., data, ...) {
#     .$informed <- FALSE
#     .super$calculate_groups(., data, ...)
#   }
#   
#   calculate <- function(., data, scales, bins = 30, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9, drop = FALSE, right = TRUE, ...) {
#     range <- scales$x$output_set()
#     
#     if (!is.null(breaks)) {
#       breaks <- interval_breaks(bins, binwidth, origin)
#     }
#     open <- if (right) "right" else "left"
# 
#     bin_interval(data$x, data$weight, breaks = breaks, open = open)
#   }
# 
#   icon <- function(.) GeomHistogram$icon()
#   default_aes <- function(.) aes(y = ..count..)
#   required_aes <- c("x")
#   default_geom <- function(.) GeomBar
#   
# })
