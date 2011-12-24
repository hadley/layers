\name{geom_ribbon}
\alias{geom_ribbon}
\title{Ribbons, y range with continuous x values}
\usage{
  geom_ribbon(aesthetics = list(), ...)
}
\description{
  Ribbons, y range with continuous x values
}
\examples{
height <- runif(10)
df <- data.frame(x = 1:10, ymax = 5 + height, ymin = 5 - height)
geom_plot(geom_ribbon(), df)
geom_plot(geom_ribbon(list(colour = "red")), df)
geom_plot(geom_ribbon(list(colour = "red", fill = NA)), df)
}
\seealso{
  \code{\link{geom_bar}}: discrete intervals (bars)

  \code{\link{geom_linerange}}: discrete intervals (lines)

  \code{\link{geom_polygon}}: general polygons
}

