# Challenge with geom_vline is that the vertical line should extend across
# the full extent of the plot - the range of the coordinate system, not the
# range of the scales.
#
# One way to solve the problem (as done by ggplot2) is to use infinties - 
# drawing from the left to right edge of the plot at the grid level.  The
# problem is that this only works for cartesian coordinate systems.
#
# Do we need sentinel values that represent special locations?
# rel(0), rel(0.5), rel(1) would always be located at the 0
# abs(1, "cm"), abs(-1, "cm")
#
# This needs enough S3 (S4?) so that units always carried along through all
# mathematical transformations, and then carefully convert to data coordinates
# at appropriate time? (Different times for rel and absolute)
#
# Is it a stat or a geom? Or a combination? 

geom_vline <- function(aesthetics = list()) {
  geom_from_call("vline")
}


geom_range.vline <- function(geom, data) {
  list(x = range(data$intercept))
}

aes_required.vline <- function(geom, data) "intercept"
aes_default.vline <- function(geom) build_defaults("line")
