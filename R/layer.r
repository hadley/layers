#' The layers package.
#' 
#' @import scales plyr
#' @docType package
#' @name layers
NULL

geom_from_layer <- function(name, call = NULL) {
  if (is.null(call)) {
    call <- match.call(sys.call(sys.parent(2)), expand.dots = FALSE)
  }
  
  aes <- call$`...`[aes_all(structure(class = name))]
    
  call$`...` <- NULL
  call$aesthetics <- do.call("call", list(as.name(list), aes))
  
  geom_from_call(name, call)
}


#' Creates layer function given a geom and stat.
#' 
#' The new function includes the arguments for geom, as well as mapping, data
#' and \code{...}.  The \code{...} arguments must be named, and the name are
#' used to determine which component (stat, adjustment or geom aesthetics) 
#' that parameter corresponds to. 
#'

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

build_geom_layer <- function(geom, stat, position) {
  
}

layer <- function() {
  
}

# Generates parameter description for a geom-layer.
rd_geom_params <- function(geom) {
  # Parse documentation and extract parameters
  # Add extra description for mapping, data, and ... (including a list of
  # all valid aesthetics)
}

build_layer <- function(mapping, data, geom, stat, adjust, params, geom.params, stat.params) {  
  args <- parse_dots(list(...), stat = stat, position = position)
  
  layer(mapping, data, 
    geom = new_stat(geom.params),
    stat = new_stat(stat, args$stat),
    adjust = new_adjust(adjust, args$adjust)
  )
}

geom_jitter    <- build_geom_layer("point", "identity", "jitter")
geom_jitter <- function(mapping, data, na.rm = FALSE, size = NULL, shape = NULL, ..., stat = "identity", position = "jitter") {
  
  build_layer(
    mapping, data,
    "point", stat, adjust,
    params = list(...),
    geom.params = list(na.rm = na.rm, size = size, shape = shape))
}

geom_bar       <- build_geom_layer("bar", "bin", "stack")
geom_histogram <- build_geom_layer("bar", "bin", "stack")