\docType{data}
\name{geom_bar}
\alias{geom_bar}
\title{Bars, rectangles with bases on x-axis.}
\format{NULL}
\usage{
  geom_bar
}
\description{
  Bars, rectangles with bases on x-axis.
}
\examples{
df <- data.frame(x = seq(0,1, 0.1), y = seq(0, 1, 0.1))
geom_plot(geom_bar(), df)
geom_plot(geom_bar(), df, munch = TRUE)
geom_plot(geom_bar(width = 0.1), df)
geom_plot(geom_bar(width = 0.05), df)
geom_plot(geom_bar(list(colour = "red")), df)
}
\seealso{
  \code{\link{stat_bin}: for more details of the binning
  algorithm #'

  \code{\link{position_dodge}}: for side-by-side barcharts

  \code{\link{position_stack}}: for more info on stacking
}
\keyword{datasets}

