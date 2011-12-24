#' The layers package.
#' 
#' @import scales plyr
#' @docType package
#' @name layers
NULL


#' Parse named list of arguments into components.
#'
#' @return a list of (up to) three components
#'   \item{geom}{parameters that belong to the geom}
#'   \item{stat}{parameters that belong to the stat}
#'   \item{adjust}{parameters that belong to the adjust}
parse_dots <- function(dots, geom = NULL, stat = NULL, adjust = NULL) {
  arg_names <- list(
    geom = names(formals(geom)),
    stat = names(formals(stat)),
    adjust = names(formals(adjust)))
  arg_df <- data.frame(
    arg = unlist(unname(arg_names)), 
    component = rep(names(arg_names), sapply(arg_names, length)))
  
  if (any(duplicated(arg_df$arg))) {
    warning("Ambiguous argument names")
  }
  
  if(!is.null(geom)) {
    aes <- aes_all(geom())
    dots$aesthetics <- dots[intersect(names(dots), aes)]
    dots <- dots[setdiff(names(dots), aes)]
  }
  
  match <- match(names(dots), arg_df$arg)
  unused <- is.na(match)
  if (any(unused)) {
    stop("Unused arguments: " , paste(names(dots)[unused], collapse = ", "),
      call. = FALSE) 
  }
  split(dots, arg_df$component[match])
}


# Generates parameter description for a geom-layer.
rd_geom_params <- function(geom) {
  # Parse documentation and extract parameters
  # Add extra description for mapping, data, and ... (including a list of
  # all valid aesthetics)
}

#' Creates layer function given a geom and stat.
#' 
#' The new function includes the arguments for geom, as well as mapping, data
#' and \code{...}.  The \code{...} arguments must be named, and the name is
#' used to determine which component (stat, adjustment or geom aesthetics) 
#' that parameter corresponds to. 
#'
build_layer <- function(geom, stat, adjust) {  
  function(...) {
    args <- parse_dots(list(...), geom = geom, stat = stat, 
      adjust = adjust)

    layer(mapping, data, 
      geom = new_geom(geom, args$geom),
      stat = new_stat(stat, args$stat),
      adjust = new_adjust(adjust, args$adjust)
    )
    
  }
}

layer_bar       <- build_layer("bar", "bin", "stack")
layer_histogram <- build_layer("bar", "bin", "stack")
layer_jitter    <- build_layer("point", "identity", "jitter")
