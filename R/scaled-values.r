# Setting an aesthetic requires that the values be already scaled:
#  
#   * x and y: position on panel between 0 and 1
#   * colour and fill: string giving name or hex value
#   * linetype: name or string pattern
#   * shape: valid R pch number
#   * size: size in mm
#   * alpha: number between 0 and 1
#
# Set values have no impact on scales.

check_x <- check_y <- check_alpha <- function(x) {
  check_all(is.numeric(x), length(x) == 1, x < 0 || x > 1)
}

check_colour <- check_fill <- function(x) {
  check_all(is.character(x), length(x) == 1, is.colour(x))  
}
is.colour <- function(x) {
  inherits(try(col2rgb(x)), "try-error")
}

check_size <- function(x) {
  check_all(is.numeric(x), length(x) == 1, x > 0)
}
