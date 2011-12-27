\name{geom_raster}
\alias{geom_raster}
\title{Raster geom.}
\usage{
  geom_raster(aesthetics = list(), na.rm = FALSE)
}
\description{
  High performance rectangular geom parameterised by
  location of center (\code{x} and \code{y}).
}
\examples{
pp <- function (n,r=4) {
 x <- seq(-r*pi, r*pi, len=n)
 df <- expand.grid(x=x, y=x)
 df$r <- sqrt(df$x^2 + df$y^2)
 df$z <- cos(df$r^2)*exp(-df$r/6)
 df
}
pp20 <- pp(20)
geom_plot(geom_raster(), pp20)
if (require("scales")) {
  pp20$fill <- cscale(pp20$z, seq_gradient_pal())
}
geom_plot(geom_raster(), pp20)
}
\seealso{
  Other rectangular.geoms: \code{\link{geom_bar}},
  \code{\link{geom_rect}}, \code{\link{geom_tile}}
}

