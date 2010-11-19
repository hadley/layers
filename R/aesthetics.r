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

  data
}

#' Check that all required aesthetics are present.
check_required_aesthetics <- function(geom, data) {
  missing_aes <- setdiff(aes_required(geom), names(data))
  if (length(missing_aes) == 0) return()

  stop(geom_name(geom), " requires the following missing aesthetics: ",
    str_c(missing_aes, collapse = ", "), call. = FALSE)
  
  data
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

#' Update data with aesthetic defaults and parameters values.
#'
#' This function is called by individual grob functions to combine data, 
#' defaults and aesthetic parameters. It also checks that all necessary
#' aesthetics are present.
#'
#' A list is returned so that constant aesthetics don't need to be needlessly
#' repeated - the recycling can occur internally in grid. \code{geom_grob}
#' methods should try and maintain this structure where possible.
#'
#' @return a list
calc_aesthetics <- function(geom, data) {
  data <- as.data.frame(data[intersect(names(data), aes_all(geom))])

  # Aesthetics parameters override data; data or parameters override defaults
  data <- modifyList(as.list(data), geom$aesthetics)
  data <- modifyList(aes_default(geom), data)
  
  check_required_aesthetics(geom, data)
  data
}