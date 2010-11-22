#' Stack overlapping objects on top of one another
#'
adjust_stack <- function(width) {
  adjust_from_call("stack")
}

position_adjust.stack <- function(adjust, data, xrange, yrange) {
  if (is.null(data$ymax) && is.null(data$y)) {
    message("Missing y and ymax in position = 'stack'. ", 
      "Maybe you want position = 'identity'?")
    return(data)
  }
  
  if (!is.null(data$ymin) && !all(data$ymin == 0)) 
    warning("Stacking not well defined when ymin != 0", call. = FALSE)

  collide(data, adjust$width, .$my_name(), pos_stack)
}

pos_stack <- function(df, width) {
  if (nrow(df) == 1) return(df)
  
  n <- nrow(df) + 1
  y <- with(df, ifelse(is.na(y), 0, y))
  if (all(is.na(df$x))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(y))
  }

  within(df, {
    ymin <- heights[-n]
    ymax <- heights[-1]
  })
}

objname <- "stack"
# icon <- function(.) {
#   y <- c(0.5, 0.8)
#   rectGrob(0.5, c(0.5, 0.8), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
# }
