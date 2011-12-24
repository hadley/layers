\name{geom_data}
\alias{geom_data}
\title{Process data for the geom.}
\usage{
  geom_data(geom, data)
}
\value{
  a list, suitable for operation with
  \code{\link{geom_data}}
}
\description{
  This method is run just prior to creating the grob, and
  is used to get the data into a format which requires
  minimal processing to be supplied to a geom. This is
  separated out into a separate method because a number of
  grobs process data in a slightly different way but
  otherwise inherit all other behaviours, and to make
  testing easier.
}
\details{
  The default behaviour uses \code{\link{calc_aesthetics}}
  to update the data with the aesthetic parameters and
  defaults stored in the geom.
}

