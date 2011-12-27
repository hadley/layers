\name{add_group}
\alias{add_group}
\title{Ensure that the data frame contains a grouping variable.}
\usage{
  add_group(data)
}
\value{
  a list with group variable
}
\description{
  If the \code{group} variable is not present, then a new
  group variable is generated from the interaction of all
  discrete (factor or character) vectors excluding label.
}

