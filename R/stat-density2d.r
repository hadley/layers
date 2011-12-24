#' 2d density estimate
#' 
#' @return \item{level}{Value of density contour, if contour = \code{TRUE}}
#' @return \item{density}{Value of density at location, if contour =
#'   \code{FALSE}}
#' @param contour If TRUE, contour the results of the 2d density estimation
#' @param n number of grid points in each direction
#' @param kde2d other arguments passed on to \code{\link{kde2d}}
#' @importFrom MASS kde2d
stat_density2d <- function(contour = TRUE, n = 100, kde2d = list()) {
  kde2d$n <- n
  rm(n)
  
  stat_from_call("density2d")
}
aes_required.density2d <- c("x", "y")

stat_transform.density2d <- function(stat, data, xrange, yrange) {  
  density <- ddply(data, "group", density2d, args = stat$kde2d)
  
  if (stat$contour) {
    contours <- stat_transform(stat_contour(), data)
    join_aesthetics(contours, data)
  } else {
    names(density)[3] <- c("density")
    join_aesthetics(density, data)
  }
}

density2d <- function(data, args = list()) {
  dens <- do.call("kde2d", c(list(x = data$x, y = data$y), args))
  data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
}
