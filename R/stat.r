stat_transform <- function(stat, data, xrange, yrange) {
  UseMethod("stat_transform")
}

stat_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  structure(arguments, class = c(name, "stat"))
}
