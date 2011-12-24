\name{stat_sum}
\alias{stat_sum}
\title{Sum unique values.  Useful for overplotting on scatterplots.}
\usage{
  stat_sum(...)
}
\value{
  \item{n} number of observations at position

  \item{prop} percent of points in that panel at that
  position
}
\description{
  Sum unique values.  Useful for overplotting on
  scatterplots.
}
\examples{
df <- data.frame(x = rpois(1e3, 3), y = rpois(1e3, 4))
locs <- stat_transform(stat_sum(), add_group(df))
names(locs)[5] <- "size"
locs$size <- sqrt(locs$size / 5)
geom_plot(geom_point(), locs)
}

