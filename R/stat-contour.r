#' Computer contours (iso-levels) of 3d data.
#'
#' @return \item{level} height of contour
#' @param bins number of contours (specify one of bins, binwidth, breaks)
#' @param binwidth distance between each contour (specify one of bins,
#'    binwidth, breaks)
#' @param breaks exact locations of contours (specify one of bins, binwidth,
#'    breaks)
#' @export
#' @S3method stat_transform contour
#' @S3method aes_required contour
#' @examples
#' if (require("reshape2")) {
#'   volcano3d <- melt(volcano)
#'   names(volcano3d) <- c("x", "y", "z")
#'   volcano3d <- add_group(volcano3d)
#'   contours <- stat_transform(stat_contour(), volcano3d)
#'   geom_plot(geom_path(), contours)
#' }
stat_contour <- function(bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE) {
  stat_from_call("contour")
}

aes_required.contour <- function(obj) c("x", "y", "z")

stat_transform.contour <- function(stat, data, xrange, yrange) {
  # If no parameters set, use pretty bins
  if (is.null(stat$bins) && is.null(stat$binwidth) && is.null(stat$breaks)) {
    breaks <- pretty(range(data$z), 10)
  } else {
    # If provided, use bins to calculate binwidth
    if (!is.null(stat$bins)) {
      binwidth <- diff(range(data$z)) / bins
    }
    # If necessary, compute breaks from binwidth
    if (is.null(stat$breaks)) {
      breaks <- fullseq(range(data$z), binwidth)
    } else {
      breaks <- stat$breaks
    }    
  }
  
  contours <- ddply(data, "group", contour, breaks = breaks)
  contours <- join_aesthetics(contours, data)
  contours$group <- id(contours[c("group", "piece")])
  contours
}

# default_aes <- function(.) aes(order = ..level..)

contour <- function(data, breaks) {
  z <- tapply(data$z, data[c("x", "y")], force)
  cl <- contourLines(
    x = sort(unique(data$x)), y = sort(unique(data$y)), z = z, 
    levels = breaks)  
  cl <- lapply(cl, list_to_df)

  contour_df <- rbind.fill(cl)
  contour_df$piece <- rep(seq_along(cl), sapply(cl, nrow))
  contour_df
}
