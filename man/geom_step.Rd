\name{geom_step}
\alias{geom_step}
\title{Connect observations with stairs.}
\usage{
  geom_step(aesthetics = list(), direction = "hv")
}
\arguments{
  \item{direction}{= "direction of stairs: 'vh' for
  vertical then horizontal, or 'hv' for horizontal then
  vertical"}
}
\description{
  Equivalent to plot(type='s').
}
\examples{
df <- data.frame(x = 1:10, y = 1:10)
geom_plot(geom_step(), df)
geom_plot(geom_step(direction = "vh"), df)
}

