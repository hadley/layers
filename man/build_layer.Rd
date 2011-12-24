\name{build_layer}
\alias{build_layer}
\title{Creates layer function given a geom and stat.}
\usage{
  build_layer(geom, stat, adjust)
}
\description{
  The new function includes the arguments for geom, as well
  as mapping, data and \code{...}.  The \code{...}
  arguments must be named, and the name is used to
  determine which component (stat, adjustment or geom
  aesthetics) that parameter corresponds to.
}

