#' Identity transformation.
#'
#' Leaves data unmodified.
#'
#' @export
stat_identity <- function() {
  stat_from_call("identity")
}

#' @S3method stat_transform identity
stat_transform.identity <- function(stat, data, ranges) {
  data
}

#' @S3method aes_required identity
aes_required.identity <- function(obj) character()

#' @S3method stat_by_group identity
stat_by_group.identity <- function(stat) TRUE