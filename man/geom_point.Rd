\name{geom_point}
\alias{geom_point}
\title{Geom: point.}
\usage{
  geom_point(aesthetics = list(), na.rm = FALSE)
}
\arguments{
  \item{na.rm}{If \code{TRUE} silently remove missing
  values, otherwise warn that they are being removed.}
}
\description{
  Geom: point.
}
\examples{
df <- data.frame(x = seq(0,1, 0.1), y = seq(0, 1, 0.1))
geom_plot(geom_point(), df)
geom_plot(geom_point(list(colour = "red")), df)
geom_plot(geom_point(list(size = 3, shape = 15)), df)

geom_plot(geom_point(list(x = 1:10, y = 10:1)))
}

