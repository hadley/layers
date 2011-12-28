#' Statistical transformations.
#'
#' Statistics need to be able to work with non-dataframe objects - this
#' is necessary for dealing with large data which might not be able to fit 
#' into memory.
#'
#' It should be easy to call statistics from outside of ggplot2, so they can
#' be tested easily and used more flexibly when ggplot2 doesn't quite do what
#' you want. For this reason, stat transforms should generally be a thin 
#' wrapper around a function that actually does the transformation. They
#' should have a common interface that specified in terms of a data frame
#' like object and variables to operate on.  Should have x range
#' (and where appropriate) y range parameters.
#'
#' The stat infrastructure should take care of preserving additional 
#' non-transformed aesthetics, processing over groups when not done by the
#' stat, and extracting ranges from scales.
#' 
#' Statistics can assume that data has already been scaled - i.e. all data
#' is now numeric, as discrete values have been converted to integers.
#' 
#' @name stat
NULL

stat_process <- function(stat, data) {
  check_required_aesthetics(stat, data)
  
  # In real use, would be provided by scales.
  ranges <- lapply(data[stat_ranges_needed(stat)], range)
  
  # data <- remove_missing(data, aes_required)
  
  data <- stat_pre_transform(stat, data, ranges)
  
  if (stat_by_group(stat)) {
    data <- stat_transform(stat, data, ranges)    
  } else {
    data <- ddply(data, "group", stat_transform, stat = stat, ranges = ranges)
  }
  data <- stat_post_transform(stat, ranges)
  
  data
}

#'
#' @param data If \code{\link{stat_by_group}} is \code{TRUE}, the method will
#'   recieve the entire data frame, otherwise it will recieve a single group
#'   at a time.  This is a performance optimisation for transformations that
#'   can operate on an entire data frame at once. These include
#'   \code{\link{stat_identity}} and \code{\link{stat_unique}}.
#'
#' @export
stat_transform <- function(stat, data, ranges) {
  UseMethod("stat_transform")
}

#' Post-transform operations.
#' 
#' @param data the complete data set
stat_post_transform <- function(stat, data, ranges) {
  UseMethod("stat_post_transform")
}
stat_post_transform.default <- function(stat, data, ranges) data

#' Pre-transform operations.
#' 
#' @param data the complete data set
stat_pre_transform <- function(stat, data, ranges) {
  UseMethod("stat_pre_transform")
}
stat_pre_transform.default <- function(stat, data, ranges) data


#' Ranged needed by the statistic.
#'
stat_ranges_needed <- function(stat) {
  UseMethod("stat_ranges_needed")
}
stat_ranges_needed.default <- function(stat) character()

#' Does the statistic operate groupwise?
#'
#'  
stat_by_group <- function(stat) UseMethod("stat_by_group")

stat_by_group.default <- function(stat) FALSE



stat_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  structure(arguments, class = c(name, "stat"))
}

#' Join constant aesthetics back onto results of statistical transformation.
#'
#' This function ensures that all group-wise constant aesthetics are 
#' preserved after statistical transformation.
join_aesthetics <- function(stats, data) {
  constant <- groupwise_constant(data, "group")
  
  join(stats, constant, by = "group", match = "first")
}

groupwise_constant <- function(df, group_var) {
  i <- which(group_var %in% names(df))
  order <- order(df[[i]])
  n <- nrow(df)

  changed <- function(x) c(TRUE, x[-1] != x[-n])
  changes <- lapply(df, function(x) changed(x[order]))

  # If col == TRUE and group == FALSE, not constant
  matching_breaks <- function(group, col) !any(col & !group)
  cols <- vapply(changes[-i], matching_breaks, group = changes[[i]], 
    FUN.VALUE = logical(1))
  df[order[changes[[i]]], c(group_var, names(which(cols))), drop = FALSE]
}
