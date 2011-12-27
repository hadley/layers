\name{stat_transform}
\alias{stat_transform}
\usage{
  stat_transform(stat, data, ranges)
}
\arguments{
  \item{data}{If \code{\link{stat_by_group}} is
  \code{TRUE}, the method will recieve the entire data
  frame, otherwise it will recieve a single group at a
  time.  This is a performance optimisation for
  transformations that can operate on an entire data frame
  at once. These include \code{\link{stat_identity}} and
  \code{\link{stat_unique}}.}
}

