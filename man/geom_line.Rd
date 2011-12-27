\name{geom_line}
\alias{geom_line}
\title{Connect observations, in ordered by x value}
\usage{
  geom_line(aesthetics = list(), arrow = NULL,
    lineend = "butt", linejoin = "round", linemitre = 1,
    na.rm = FALSE, ...)
}
\description{
  Connect observations, in ordered by x value
}
\examples{
df <- data.frame(x = sample(1:10), y = sample(1:10))
geom_plot(geom_line(), df)
geom_plot(geom_line(list(colour = "red")), df)
geom_plot(geom_line(list(size = 3)), df)
}
\seealso{
  Other line geoms: \code{\link{geom_path}},
  \code{\link{geom_segment}}, \code{\link{geom_step}}
}

