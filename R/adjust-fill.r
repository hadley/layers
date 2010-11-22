#' Stack overlapping objects on top of one another, and standardise 
#' to have equal height.
#'
adjust_fill <- function(width = NULL) {
  adjust_from_call()
}

adjust_position.fill <- function(adjust, data, xrange, yrange) {
  if (empty(data)) return(data.frame())
  
  check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
  if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
  collide(data, adjust$width, "fill", pos_fill)
}

pos_fill <- function(df, width) {
  within(pos_stack(df, width), {
    ymin <- ymin / max(ymax)
    ymax <- ymax / max(ymax)
  })
}

rescale.fill <- function(stat) TRUE

# icon <- function(.) {
#   y <- c(0.5, 0.8)
#   rectGrob(0.5, c(0.625, 1), width=0.4, height=c(0.625, 0.375), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
# }
