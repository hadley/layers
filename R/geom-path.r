#' Connect observations, in original order.
#' 
#' Lines containing less than two observations will be silently dropped.
#'
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @param linemitre Line mitre limit (number greater than 1)
#' @param arrow Arrow specification, as created by ?arrow
#' @family line geoms
#' @export
#' @examples
#' df <- data.frame(x = sample(1:10), y = sample(1:10))
#' geom_plot(geom_path(), df)
#' geom_plot(geom_path(list(colour = "red")), df)
#' geom_plot(geom_path(list(size = 3)), df)
#'
#' geom_plot(geom_path(list(x = 1:10, y = 10:1)))
geom_path <- function(aesthetics = list(), arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE) {
  geom_from_call("path")
}

# Aesthetics -----------------------------------------------------------------

#' @S3method aes_required path
aes_required.path <- function(geom) c("x", "y")

#' @S3method aes_default path
aes_default.path <- function(geom) build_defaults("line")

#' @S3method aes_icon path
aes_icon.path <- function(geom) {
  data.frame(
    x = c(0.2, 0.4, 0.8, 0.6, 0.5), 
    y = c(0.2, 0.7, 0.4, 0.1, 0.5))
}

# Data and munching ----------------------------------------------------------

#' @S3method geom_data path
geom_data.path <- function(geom, data) {
  data <- list_to_df(data)
  data <- remove_missing(data, na.rm = geom$na.rm)
  
  # Silently drop lines with less than two points, preserving order
  rows <- ave(seq_len(nrow(data)), data$group, FUN = length)
  data <- data[rows >= 2, ]
  
  data
}

# Remove missing values at the start or end of a line - can't drop in the 
# middle since you expect those to be shown by a break in the line
remove_missing <- function(data, na.rm = FALSE) {
  keep <- function(x) {
    # from first non-missing to last non-missing
    first <- match(FALSE, x, nomatch = 1) - 1
    last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
    c(
      rep(FALSE, first), 
      rep(TRUE, last - first), 
      rep(FALSE, length(x) - last))
  }    

  missing <- !complete.cases(data[c("x", "y", "size", "colour",
    "linetype")])
  kept <- ave(missing, data$group, FUN=keep)
  data <- data[kept, ]
  
  if (!all(kept) && !geom$na.rm) {
    warning("Removed ", sum(!kept), " rows containing missing values", 
      " (geom_path).", call. = FALSE)
  }
  
  data
}


# Work out whether we should use lines or segments
path_constant_aesthetics <- function(data) {

  # # Possibly faster approach:
  # solid <- vaggregate(data$linetype, data$group, function(x) all(x == 1))
  # id <- id(df[, c("alpha", "colour","size", "linetype")])
  # constant <- vaggregate(id, data$group, function(x) length(unique(x)) == 1)
  attr <- ddply(data, .(group), function(df) {
    data.frame(
      solid = identical(unique(df$linetype), 1),
      constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
    )
  })
  solid_lines <- all(attr$solid)
  constant <- all(attr$constant)
  if (!solid_lines && !constant) {
    stop("geom_path: If you are using dotted or dashed lines", 
      ", colour, size and linetype must be constant over the line",
      call.=FALSE)
  }
  
  constant
}


# Drawing --------------------------------------------------------------------

#' @S3method geom_grob path
geom_grob.path <- function(geom, data) {
  data <- list_to_df(data)
  if (nrow(data) < 2) return(zeroGrob())

  # Work out grouping variables for grobs
  n <- nrow(data)
  group_diff <- data$group[-1] != data$group[-n]
  start <- c(TRUE, group_diff)
  end <-   c(group_diff, TRUE)
  
  if (path_constant_aesthetics(data)) {
    # Lines have the same aesthetics their whole length
    polylineGrob(
      data$x, data$y, id = as.integer(factor(data$group)), 
      default.units = "native", arrow = data$arrow,
      gp = gpar(
        col = alpha(data$colour, data$alpha)[start], 
        lwd = data$size[start] * .pt, lty = data$linetype[start], 
        lineend = geom$lineend, linejoin = geom$linejoin, 
        linemitre = geom$linemitre)
    )
  } else {
    # Segments have varying aesthetics, but don't work with non-solid lines
    # because the dash pattern is broken
    segmentsGrob(
      data$x[!end], data$y[!end], data$x[!start], data$y[!start],
      default.units="native", arrow = data$arrow,
      gp = gpar(
        col = alpha(data$colour, data$alpha)[!end], 
        lwd = data$size[!end] * .pt, lty = data$linetype[!end], 
        lineend = geom$lineend, linejoin = geom$linejoin, 
        linemitre = geom$linemitre
      )
    )
  }
}



