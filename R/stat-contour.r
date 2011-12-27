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

#' @S3method stat_transform contour
stat_transform.contour <- function(stat, data, ranges) {
  breaks <- contour_breaks(ranges$z, stat$breaks, stat$width, stat$n)
  contour_data(data, breaks)
}

#' @S3method stat_post_transform contour
stat_post_transform.contour <- function(stat, data, ranges) {
  data$group <- id(data[c("group", "piece")])
  data
}

#' @S3method stat_ranges_needed contour
stat_ranges_needed.default <- function(stat) "z"

#' @S3method aes_required contour
aes_required.contour <- function(obj) c("x", "y", "z")

# Implementation of statistic ------------------------------------------------

#' @importFrom scales fullseq
contour_breaks <- function(range, breaks = NULL, width = NULL, n = 10) {
  if (!is.null(breaks)) return(breaks)
  
  if (is.null(width)) {
    width <- pretty(range, n)
  }
  
  fullseq(range(data$z), width)
}

#' @importFrom reshape2 acast
#' @importFrom plyr rbind.fill
#' @importFrom grDevices contourLines
contour_data <- function(data, vars, breaks) {
  z <- acast(data, vars[1:2], value.var = vars[3])
  
  cl <- contourLines(
    x = sort(unique(data[[vars[1]]])),
    y = sort(unique(data[[vars[2]]])),
    z = z, 
    levels = breaks)
  
  cl <- lapply(cl, list_to_df)

  contour_df <- rbind.fill(cl)
  contour_df$piece <- rep(seq_along(cl), vapply(cl, nrow, integer(1)))
  contour_df
}
