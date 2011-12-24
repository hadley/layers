\name{geom_text}
\alias{geom_text}
\title{Textual annotations}
\usage{
  geom_text(aesthetics = list(), parse = FALSE,
    fontfamily = NULL, fontface = NULL, lineheight = NULL)
}
\arguments{
  \item{parse}{If \code{TRUE}, the labels will be parsed
  into expressions and displayed as described in
  \code{\link{plotmath}}}

  \item{fontfamily}{}

  \item{fontface}{character string specifying plain, bold,
  italic etc.}

  \item{lineheight}{line height as multiple of text height}
}
\description{
  Textual annotations
}
\examples{
df <- data.frame(x = 1:5, y = 1:5, label = letters[1:5])
geom_plot(geom_text(), df)
geom_plot(geom_text(list(angle = 45)), df)
geom_plot(geom_text(fontface = "bold"), df)
geom_plot(geom_text(fontfamily = "Times New Roman"), df)
}

