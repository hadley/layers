\name{stat_density}
\alias{stat_density}
\title{1d density estimation.}
\usage{
  stat_density(adjust = 1, kernel = "gaussian",
    bw = "nrd0", n = 512)
}
\arguments{
  \item{adjust}{see ?density for details}

  \item{kernel}{string giving kernel used for density
  estimation, see \\code{\\link{density}} for details"}

  \item{bw}{the smoothing bandwidth to be used, or a
  character vector giving the name of an algorithm to
  calculate it automatically}

  \item{n}{number of points in output}
}
\value{
  \itemize{ \item{density}{density estimate}
  \item{count}{density * number of points, an approximate
  count, and useful for stacked density plots}
  \item{scaled}{density estimate, scaled to maximum of 1} }
}
\description{
  Groups containing three observations or less are silently
  dropped.
}
\seealso{
  \code{\link{stat_bin}} for the histogram,
  \code{\link{density}} for details of the algorithm used
}

