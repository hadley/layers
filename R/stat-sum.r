#' Sum unique values.  Useful for overplotting on scatterplots.
#'
#' @return \item{n} number of observations at position
#' @return \item{prop} percent of points in that panel at that position
#'
#' @export
#' @S3method stat_transform sum
#' @examples
#' df <- data.frame(x = rpois(1e3, 3), y = rpois(1e3, 4))
#' locs <- stat_transform(stat_sum(), add_group(df))
#' names(locs)[5] <- "size"
#' locs$size <- sqrt(locs$size / 5)
#' geom_plot(geom_point(), locs)
stat_sum <- function(...) {
  stat_from_call("sum")
}

stat_transform.sum <- function(stat, data, xrange, yrange) {
  if (is.null(data$weight)) data$weight <- 1
  
  counts <- ddply(data, .(x, y, group), function(df) {
    cols <- names(df)[sapply(df, function(x) length(unique(x)) == 1)]
    data.frame(df[1, cols, drop = FALSE], n = sum(df$weight))
  })
  counts$prop <- ave(counts$n, counts$group, FUN = function(x) x / sum(x))
  counts

  # counts <- count(data, c("x", "y", "group"), "weight")
  # counts$prop <- ave(counts$n, counts$group, FUN = function(x) x / sum(x))
  # join_aesthetics(counts, data)
}
