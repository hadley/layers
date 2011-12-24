\name{parse_dots}
\alias{parse_dots}
\title{Parse named list of arguments into components.}
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
  Parse named list of arguments into components.
}

