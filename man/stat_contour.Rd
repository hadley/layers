\name{stat_contour}
\alias{stat_contour}
\title{Computer contours (iso-levels) of 3d data.}
\usage{
  stat_contour(bins = NULL, binwidth = NULL, breaks = NULL,
    na.rm = FALSE)
}
\arguments{
  \item{bins}{number of contours (specify one of bins,
  binwidth, breaks)}

  \item{binwidth}{distance between each contour (specify
  one of bins, binwidth, breaks)}

  \item{breaks}{exact locations of contours (specify one of
  bins, binwidth, breaks)}
}
\value{
  \item{level} height of contour
}
\description{
  Computer contours (iso-levels) of 3d data.
}
\examples{
if (require("reshape2")) {
  volcano3d <- melt(volcano)
  names(volcano3d) <- c("x", "y", "z")
  volcano3d <- add_group(volcano3d)
  contours <- stat_transform(stat_contour(), volcano3d)
  geom_plot(geom_path(), contours)
}
}

