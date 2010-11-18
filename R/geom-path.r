#' Connect observations, in original order.
#' 
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @param linemitre Line mitre limit (number greater than 1)
#' @param arrow Arrow specification, as created by ?arrow
#' @seealso \code{\link{geom_line}}: functional (ordered) lines, 
#' @seealso \code{\link{geom_polygon}}: filled paths (polygons), 
#' @seealso \code{\link{geom_segment}}: line segments
#' @examples
#' # Generate data
#' myear <- ddply(movies, .(year), colwise(mean, .(length, rating)))
#' p <- ggplot(myear, aes(length, rating))
#' p + geom_path()
#' 
#' # Add aesthetic mappings
#' p + geom_path(aes(size = year))
#' p + geom_path(aes(colour = year))
#' 
#' # Change scale
#' p + geom_path(aes(size = year)) + scale_size(to = c(1, 3))
#' 
#' # Set aesthetics to fixed value
#' p + geom_path(colour = "green")
#' 
#' # Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_path(size = 10)
#' base + geom_path(size = 10, lineend = "round")
#' base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")
#' 
#' # Use qplot instead
#' qplot(length, rating, data=myear, geom="path")
#' 
#' # Using economic data:
#' # How is unemployment and personal savings rate related?
#' qplot(unemploy/pop, psavert, data=economics)
#' qplot(unemploy/pop, psavert, data=economics, geom="path")
#' qplot(unemploy/pop, psavert, data=economics, geom="path", size=as.numeric(date))
#' 
#' # How is rate of unemployment and length of unemployment?
#' qplot(unemploy/pop, uempmed, data=economics)
#' qplot(unemploy/pop, uempmed, data=economics, geom="path")
#' qplot(unemploy/pop, uempmed, data=economics, geom="path") +
#'   geom_point(data=head(economics, 1), colour="red") + 
#'   geom_point(data=tail(economics, 1), colour="blue")
#' qplot(unemploy/pop, uempmed, data=economics, geom="path") +
#'   geom_text(data=head(economics, 1), label="1967", colour="blue") + 
#'   geom_text(data=tail(economics, 1), label="2007", colour="blue")
#' 
#' # geom_path removes missing values on the ends of a line.
#' # use na.rm = T to suppress the warning message
#' df <- data.frame(
#'   x = 1:5,
#'   y1 = c(1, 2, 3, 4, NA),
#'   y2 = c(NA, 2, 3, 4, 5),
#'   y3 = c(1, 2, NA, 4, 5),
#'   y4 = c(1, 2, 3, 4, 5))
#' qplot(x, y1, data = df, geom = c("point", "line"))
#' qplot(x, y2, data = df, geom = c("point", "line"))
#' qplot(x, y3, data = df, geom = c("point", "line"))
#' qplot(x, y4, data = df, geom = c("point", "line"))
#' 
#' # Setting line type vs colour/size
#' # Line type needs to be applied to a line as a whole, so it can
#' # not be used with colour or size that vary across a line
#' 
#' x <- seq(0.01, .99, length=100)
#' df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#' 
#' # Should work
#' p + geom_line(linetype = 2)
#' p + geom_line(aes(colour = group), linetype = 2)
#' p + geom_line(aes(colour = x))
#' 
#' # Should fail
#' should_stop(p + geom_line(aes(colour = x), linetype=2))
layer_path <- function(arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  new_layer(..., geom = geom_from_layer("path"))
}

geom_path <- function(aesthetics = list(), arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) {
  geom_from_call("path")
}

geom_grob.path <- function(geom, data, ...) {
  if (nrow(data) < 2) return(zeroGrob())

  data <- calc_aesthetics(geom, data)

  # Work out grouping variables for grobs
  n <- nrow(data)
  group_diff <- data$group[-1] != data$group[-n]
  start <- c(TRUE, group_diff)
  end <-   c(group_diff, TRUE)
  
  if (constant_aesthetics(data)) {
    # Lines have the same aesthetics their whole length
    with(data, 
      polylineGrob(
        x, y, id = as.integer(factor(group)), 
        default.units = "native", arrow = arrow, ...,
        gp = gpar(
          col = alpha(colour, alpha)[start], 
          lwd = size[start] * .pt, lty = linetype[start], 
          lineend = geom$lineend, linejoin = geom$linejoin, 
          linemitre = geom$linemitre)
      )
    )
  } else {
    # Segments have varying aesthetics, but don't work with non-solid lines
    # because the dash pattern is broken
    with(data, 
      segmentsGrob(
        x[!end], y[!end], x[!start], y[!start],
        default.units="native", arrow = arrow, ...,
        gp = gpar(
          col = alpha(colour, alpha)[!end], 
          lwd = size[!end] * .pt, lty = linetype[!end], 
          lineend = geom$lineend, linejoin = geom$linejoin, 
          linemitre = geom$linemitre
        )
      )
    )
  }
}

remove_missing <- function(geom, data) {

  keep <- function(x) {
    # from first non-missing to last non-missing
    first <- match(FALSE, x, nomatch = 1) - 1
    last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
    c(
      rep(FALSE, first), 
      rep(TRUE, last - first), 
      rep(FALSE, length(x) - last))
  }    
  # Drop missing values at the start or end of a line - can't drop in the 
  # middle since you expect those to be shown by a break in the line
  missing <- !complete.cases(data[c("x", "y", "size", "colour",
    "linetype")])
  kept <- ave(missing, data$group, FUN=keep)
  data <- data[kept, ]
  
  if (!all(kept) && !geom$na.rm) {
    warning("Removed ", sum(!kept), " rows containing missing values", 
      " (geom_path).", call. = FALSE)
  }
  
  # Silently drop lines with less than two points, preserving order
  rows <- ave(seq_len(nrow(data)), data$group, FUN = length)
  data <- data[rows >= 2, ]
  
  data
}


# Work out whether we should use lines or segments
constant_aesthetics <- function(data) {

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

aes_required.path <- function(geom) c("x", "y")
aes_present.path <- function(geom) c()
aes_default.path <- function(geom) {
  list(colour = "black", size = 0.5, linetype = 1, alpha = 1)
}


geom_visualize.path <- function(geom, data = list()) {
  defaults <- list(
    x = c(0.2, 0.4, 0.8, 0.6, 0.5), 
    y = c(0.2, 0.7, 0.4, 0.1, 0.5))
  data <- modifyList(defaults, data)

  geom_grob(geom, data, default.units = "npc")
}
