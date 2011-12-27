\name{calc_aesthetics}
\alias{calc_aesthetics}
\title{Update data with aesthetic defaults and parameters values.}
\usage{
  calc_aesthetics(geom, data)
}
\value{
  a list
}
\description{
  This function is called by individual grob functions to
  combine data, defaults and aesthetic parameters. It
  throws away all aesthetics not used by the geom, but does
  not check that all required aesthetics are present: these
  may be added by \code{\link{geom_data}} which is called
  after \code{calc_aesthetics}.
}
\details{
  A list is returned so that constant aesthetics don't need
  to be needlessly repeated - the recycling can occur
  internally in grid. \code{geom_grob} methods should try
  and maintain this structure where possible.
}

