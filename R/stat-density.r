#' 1d density estimation.
#'
#' Groups containing three observations or less are silently dropped.
#'
#' @param adjust see ?density for details
#' @param kernel string giving kernel used for density estimation, see
#'   \\code{\\link{density}} for details"
#' @param bw the smoothing bandwidth to be used, or a character vector
#'   giving the name of an algorithm to calculate it automatically
#' @param n number of points in output
#' @export
#' @S3method stat_transform density
#' @S3method aes_required density
#' @S3method aes_present density
#' @return \itemize{
#'   \item{density}{density estimate}
#'   \item{count}{density * number of points, an approximate count, and
#'     useful for stacked density plots}
#'    \item{scaled}{density estimate, scaled to maximum of 1}
#' }
#' @seealso \code{\link{stat_bin}} for the histogram, 
#'  \code{\link{density}} for details of the algorithm used
stat_density <- function(adjust = 1, kernel = "gaussian", bw = "nrd0", n = 512) {
  stat_from_call("density")
}

stat_transform.density <- function(stat, data, xrange, yrange) {
  results <- ddply(data, "group", density, stat = stat, xrange = xrange)
  join_aesthetics(results, data)
}

density <- function(stat, data, xrange) {  
  n <- nrow(data)
  if (n < 3) return(data.frame())
 
  dens <- stats::density(data$x, weights = data$weight,
    adjust = stat$adjust, kernel = stat$kernel, bw = stat$bw, n = stat$n, 
    from = xrange[1], to = xrange[2])

  densdf <- data.frame(x = dens$x, density = dens$y)
  densdf$scaled <- densdf$y / max(densdf$y, na.rm = TRUE)
  densdf$count <- densdf$y * n
  densdf
}

aes_required.density <- function(obj) "x"
aes_present.density <- function(obj) "weight"