\name{geom_segment}
\alias{geom_segment}
\title{Single line segments.}
\usage{
  geom_segment(aesthetics = list(), arrow = NULL)
}
\arguments{
  \item{arrow}{specification for arrow heads, as created by
  arrow()}
}
\description{
  Single line segments.
}
\examples{
df <- data.frame(x = runif(20), y = runif(20))
df <- transform(df,
  xend = x + rnorm(20, sd = 1/5),
  yend = y + rnorm(20, sd = 1/5))
geom_plot(geom_segment(), df)
geom_plot(geom_segment(), df, munch = TRUE)
geom_plot(geom_segment(arrow = arrow()), df)
}
\seealso{
  \code{\link{geom_path}} connect multiple points

  \code{\link{geom_line}} connect points ordered along x.
}

