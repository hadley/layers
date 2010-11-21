#' Identity transformation
#'
#' @export
#' @S3method stat_transform identity
#' @S3method aes_required identity
stat_identity <- function() {
  stat_from_call("identity")
}

stat_transform.density <- function(stat, data, xrange, yrange) {
  data
}

aes_required.identity <- function(obj) c()
