\name{geom_grob}
\alias{geom_grob}
\title{Render a grid grob from a geom and a dataset.}
\usage{
  geom_grob(geom, data)
}
\value{
  a grob
}
\description{
  This is the key method to implement when creating a new
  geom.  Given a geom and its paramters, and a dataset, it
  renders the data to produce a grid grob. The data
  supplied to this function has already been scaled, so all
  values are interpretable by grid, and data and aesthetics
  have been combined into a single list by
  \code{\link{geom_draw}}
}

