\name{geom_premunch}
\alias{geom_premunch}
\title{Prepare the data for munching (if needed).}
\usage{
  geom_premunch(geom, data)
}
\value{
  list containing \itemize{ \item{data}{the munched data}
  \item{geom}{the new geom} }
}
\description{
  This function will only be run if the coordinate system
  is non-linear, and requires "munching" (breaking the data
  into small enough pieces that they will still be linear
  after transformation).  This method doesn't actually do
  the munching - it just gets the data into a standard form
  where it can happen.
}
\details{
  This usually requires reparameterising the geom to by
  something that can be drawn by \code{\link{geom_path}} or
  \code{\link{geom_polygon}}, and so the function should
  return both the data and the new geom to be used to draw
  the data.

  The default method leaves the data and geom unchanged.
}

