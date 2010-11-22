adjust_position <- function(stat, data, xrange, yrange) {
  UseMethod("adjust_position")
}

adjust_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  structure(arguments, class = c(name, "adjust"))
}
