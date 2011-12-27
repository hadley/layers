\name{geom_bar}
\alias{geom_bar}
\title{Bars, rectangles with bases on x-axis.}
\usage{
  geom_bar(aesthetics = list(), width = NULL,
    na.rm = FALSE, ...)
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
geom_plot(geom_bar(list(colour = "black")), df)
geom_plot(geom_bar(list(fill = "red")), df)
}
\seealso{
  Other rectangular geoms: \code{\link{geom_rect}},
  \code{\link{geom_tile}}
}

