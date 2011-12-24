\name{aes_present}
\alias{aes_present}
\title{Aesthetics that should be non-missing.}
\usage{
  aes_present(geom)
}
\value{
  character vector of aesthetic names
}
\description{
  Present aesthetics must be non-missing - rows of data
  that have missing values for these aesthetics are removed
  with a warning.
}

