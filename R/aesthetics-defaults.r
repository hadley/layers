#' Default aesthetics.
#'
#' These values are used to fill in missing aesthetics to provide default
#' values. See \code{\link{build_defaults}} for a way to build defaults from
#' a common list of options.
#' 
#' @return named list of default values
#' @export
aes_default <- function(geom) UseMethod("aes_default")

overall_defaults <- list(
  point = list(colour = "black", size = 2, shape = 19, alpha = 1, fill = NA),
  line = list(colour = "black", size = 0.5, linetype = 1, alpha = 1),
  solid = list(colour = NA, fill = "grey20", alpha = 1),
  overlay = list(colour = "#3366FF", fill = "grey20", alpha = 0.4)
)

#' Build set of defaults from named types.
#'
#' @export
build_defaults <- function(types) {
  aesthetics <- overall_defaults[types] 
  if (length(aesthetics) == 1) return(aesthetics[[1]])

  Reduce(modifyList, aesthetics[-1], init = aesthetics[[1]])
}
