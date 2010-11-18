#' Required aesthetics.
#'
#' @return character vector of aesthetic names
aes_required <- function(geom) UseMethod("aes_required")

#' Aesthetics that should be non-missing.
#'
#' Leave blank if you want to do your own checking.  See \code{geom_line} 
#' for an example.
#' @return character vector of aesthetic names
aes_present <- function(geom) UseMethod("aes_present")

#' Default aesthetics
#'
#' @return named list of default values
aes_default <- function(geom) UseMethod("aes_default")

#' All aesthetics
aes_all <- function(geom) c(aes_required(geom), names(aes_default(geom)))


overall_defaults <- list(
  point = list(colour = "black", size = 2, shape = 19, alpha = 1, fill = NA),
  line = list(colour = "black", size = 0.5, linetype = 1, alpha = 1),
  solid = list(colour = NA, fill = "grey20", alpha = 1),
  overlay = list(colour = "#3366FF", fill = "grey20", alpha = 0.4)
)
build_defaults <- function(types) {
  aesthetics <- overall_defaults[types] 
  if (length(aesthetics) == 1) return(aesthetics[[1]])

  Reduce(modifyList, aesthetics[-1], init = aesthetics[[1]])
}

check_set_aesthetics <- function(geom) {
  correct <- names(geom$aesthetics) %in% aes_all(geom)
  if (all(correct)) return()
    
  stop(geom_name(geom), " does not have aesthetics ", 
    names(geom$aesthetics)[!correct])
}

#' Check that all required aesthetics are present.
check_required_aesthetics <- function(geom, data) {
  missing_aes <- setdiff(aes_required(geom), names(data))
  if (length(missing_aes) == 0) return()

  stop(geom_name(geom), " requires the following missing aesthetics: ",
    str_c(missing_aes, collapse = ", "), call. = FALSE)
  
}

#' Should be called once for each layer, not once for each panel.
check_missing_aesthetics <- function(geom, data) {
  vars <- intersect(aes_present(geom), names(data))
  complete <- complete.cases(data[, vars])
  if (all(complete)) return(data)
  
  data <- data[complete, ]
  if (!geom$na.rm) {
    warning("Removed ", sum(missing), " rows containing missing values in ",
      geom_name(geom), ".", call. = FALSE)
  }
  data
}

#' Update data with defaults and set values.
#'
#' Also checks whether or necessary aesthetics are present.
#' @return a data frame
calc_aesthetics <- function(geom, data) {
  # Check required aesthetics are present, and remove rows with missing
  # values.
  check_required_aesthetics(geom, data)
  data <- check_missing_aesthetics(geom, data)
  
  # Set aesthetics override mapped aesthetics in data.
  data <- modifyList(data, geom$aesthetics)
  
  # Set or mapped aesthetics override defaults, and make sure no extraneous
  # columns remain.
  modifyList(aes_default(geom), data)[aes_all(geom)]
}