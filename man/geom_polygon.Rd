\name{geom_polygon}
\alias{geom_polygon}
\title{Polygon, a filled path.}
\usage{
  geom_polygon(aesthetics = list())
}
\description{
  Polygon, a filled path.
}
\examples{
if (require(maps)) {
  ni <- data.frame(map("nz", "North.Island", plot = F)[c("x", "y")])
  si <- data.frame(map("nz", "South.Island", plot = F)[c("x", "y")])
  geom_plot(geom_polygon(), ni)
  geom_plot(geom_polygon(list(colour = "grey50", fill = NA)), si)

  nz <- rbind(data.frame(ni, group = 1), data.frame(si, group = 2))
  geom_plot(geom_polygon(), nz)
}
}
\seealso{
  \code{\link{geom_path}}: an unfilled polygon

  \code{\link{geom_ribbon}}: a polygon anchored on the
  x-axis
}

