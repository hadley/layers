#' Default related stat and adjust.
#'
#' The defaults are the identity functions which leave the data unchanged.
#' 
#' @aliases geom_stat geom_adjust
#' @export geom_stat geom_adjust
#' @S3method geom_stat default
#' @S3method geom_adjust default
#' @usage geom_stat(geom, ...)
#' @usage geom_adjust(geom, ...)
#' @param ... Other arguments passed on to the object creation function.  
#'   These are used when initialised a geom from a name and list of 
#'   parameters, which may belong to the stat or the position adjustment.
geom_stat <- function(geom, ...) UseMethod("geom_stat")
geom_adjust <- function(geom, ...) UseMethod("geom_adjust")

geom_stat.default <- function(geom, ...) stat_identity()
geom_adjust.default <- function(geom, ...) adjust_identity()

#' @S3method geom_adjust bar
#' @S3method geom_stat bar
geom_stat.bar <- function(geom, ...) stat_bin(...)
geom_adjust.bar <- function(geom, ...) adjust_stack(...)
