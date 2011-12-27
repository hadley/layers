\name{geom_path}
\alias{geom_path}
\title{Connect observations, in original order.}
\usage{
  geom_path(aesthetics = list(), arrow = NULL,
    lineend = "butt", linejoin = "round", linemitre = 1,
    na.rm = FALSE, ...)
}
\arguments{
  \item{lineend}{Line end style (round, butt, square)}

  \item{linejoin}{Line join style (round, mitre, bevel)}

  \item{linemitre}{Line mitre limit (number greater than
  1)}

  \item{arrow}{Arrow specification, as created by ?arrow}
}
\description{
  Lines containing less than two observations will be
  silently dropped.
}
\examples{
df <- data.frame(x = sample(1:10), y = sample(1:10))
geom_plot(geom_path(), df)
geom_plot(geom_path(list(colour = "red")), df)
geom_plot(geom_path(list(size = 3)), df)

geom_plot(geom_path(list(x = 1:10, y = 10:1)))
}
\seealso{
  Other line geoms: \code{\link{geom_line}},
  \code{\link{geom_segment}}, \code{\link{geom_step}}
}

