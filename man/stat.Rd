\name{stat}
\alias{stat}
\title{Statistical transformations.}
\description{
  Statistics need to be able to work with non-dataframe
  objects - this is necessary for dealing with large data
  which might not be able to fit into memory.
}
\details{
  It should be easy to call statistics from outside of
  ggplot2, so they can be tested easily and used more
  flexibly when ggplot2 doesn't quite do what you want. For
  this reason, stat transforms should generally be a thin
  wrapper around a function that actually does the
  transformation. They should have a common interface that
  specified in terms of a data frame like object and
  variables to operate on.  Should have x range (and where
  appropriate) y range parameters.

  The stat infrastructure should take care of preserving
  additional non-transformed aesthetics, processing over
  groups when not done by the stat, and extracting ranges
  from scales.

  Statistics can assume that data has already been scaled -
  i.e. all data is now numeric, as discrete values have
  been converted to integers.
}

