\name{stat_density2d}
\alias{stat_density2d}
\title{2d density estimate}
\usage{
  stat_density2d(contour = TRUE, n = 100, kde2d = list())
}
\arguments{
  \item{contour}{If TRUE, contour the results of the 2d
  density estimation}

  \item{n}{number of grid points in each direction}

  \item{kde2d}{other arguments passed on to
  \code{\link{kde2d}}}
}
\value{
  \item{level}{Value of density contour, if contour =
  \code{TRUE}}

  \item{density}{Value of density at location, if contour =
  \code{FALSE}}
}
\description{
  2d density estimate
}

