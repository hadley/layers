#' Required aesthetics.
#'
#' Required aesthetics must be present, otherwise the geom can not be drawn.
#' These are usually position aesthetics, as all others are supplied by the
#' defaults.
#'
#' @return character vector of aesthetic names
#' @export
aes_required <- function(geom) UseMethod("aes_required")

#' Aesthetics that should be non-missing.
#'
#' Present aesthetics must be non-missing - rows of data that have missing
#' values for these aesthetics are removed with a warning. 
#'
#' @return character vector of aesthetic names
#' @export
aes_present <- function(geom) UseMethod("aes_present")

#' Default aesthetics for drawing representation icons.
#'
#' @return list of aesthetics
#' @export
aes_icon <- function(geom) UseMethod("aes_icon")

#' All aesthetics
#'
#' @export
aes_all <- function(geom) {
  c("group", aes_required(geom), names(aes_default(geom)))
}

#' Check that aesthetic parameters are for correct aesthetics.
check_aesthetic_params <- function(geom, aesthetics) {
  correct <- names(aesthetics) %in% aes_all(geom)
  if (all(correct)) return()
    
  stop(geom_name(geom), " does not have aesthetics ", 
    names(aesthetics)[!correct])
}

#' Check that all required aesthetics are present.
check_required_aesthetics <- function(geom, data) {
  missing_aes <- setdiff(aes_required(geom), names(data))
  if (length(missing_aes) == 0) return()

  stop(geom_name(geom), " requires the following missing aesthetics: ",
    paste(missing_aes, collapse = ", ", sep = ""), call. = FALSE)
  
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
  data <- as.data.frame(data[intersect(names(data), aes_all(geom))], 
    stringsAsFactors = FALSE)

  # Aesthetics parameters override data; data or parameters override defaults
  data <- modifyList(as.list(data), geom$aesthetics)
  data <- modifyList(aes_default(geom), data)
  
  check_required_aesthetics(geom, data)
  data
}

# all_aes <- function(y) c(names(y$default_aes()), y$required_aes)
# geom_aes <- unlist(lapply(Geom$find_all(), all_aes))
# stat_aes <- unlist(lapply(Stat$find_all(), all_aes))
# all <- sort(unique(c(names(.base_to_ggplot), geom_aes, stat_aes)))
# dput(all)
all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", "max", "middle", "min", "order", "pch", "radius", "sample", "shape", "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", "y", "yend", "ymax", "ymin", "z")

constant_aesthetics <- function(data, exclude = c()) {
  aes <- data[intersect(names(data), setdiff(all_aesthetics, exclude))]
  aes <- aes[!duplicated(id(aes)), ]
  aes
}
