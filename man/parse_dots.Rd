\name{parse_dots}
\alias{parse_dots}
\title{Creates layer function given a geom and stat.}
\usage{
  parse_dots(dots, geom = NULL, stat = NULL, adjust = NULL)
}
\value{
  a list of (up to) three components \item{geom}{parameters
  that belong to the geom} \item{stat}{parameters that
  belong to the stat} \item{adjust}{parameters that belong
  to the adjust}
}
\description{
  The new function includes the arguments for geom, as well
  as mapping, data and \code{...}.  The \code{...}
  arguments must be named, and the name are used to
  determine which component (stat, adjustment or geom
  aesthetics) that parameter corresponds to.
}
\details{
  Parse named list of arguments into components.
}

