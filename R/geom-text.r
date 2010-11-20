#' Textual annotations
#' 
#' @param parse If \code{TRUE}, the labels will be parsed into expressions 
#'   and displayed as described in \code{\link{plotmath}}
#' @param fontfamily
#' @param fontface character string specifying plain, bold, italic etc.
#' @param lineheight line height as multiple of text height
#' @export
#' @S3method aes_required text
#' @S3method aes_defaults text
#' @S3meothd geom_grob text
#' @S3meothd geom_visaulise text
#' @example
#' df <- data.frame(x = 1:5, y = 1:5, label = letters[1:5])
#' geom_plot(geom_text(), df)
#' geom_plot(geom_text(list(angle = 45)), df)
#' geom_plot(geom_text(fontface = "bold"), df)
#' geom_plot(geom_text(fontfamily = "Times New Roman"), df)
geom_text <- function(aesthetics = list(), parse = FALSE, fontfamily = NULL, fontface = NULL, lineheight = NULL) {
  geom_from_call("text")
}

aes_required.text <- function(geom) c("x", "y", "label")
aes_default.text <- function(geom) {
  list(colour = "black", size = 5 , angle = 0, hjust = 0.5, vjust = 0.5, 
    alpha = 1)
}

geom_grob.text <- function(geom, data, ...) {
  label <- data$label
  if (geom$parse) {
    label <- parse(text = label)
  }

  textGrob(label, data$x, data$y, default.units = "native", 
    hjust = data$hjust, vjust = data$vjust, rot = data$angle, 
    gp = gpar(col = alpha(data$colour, data$alpha), 
    fontsize = data$size * .pt, fontfamily = geom$fontfamily,
    fontface = geom$fontface, lineheight = geom$lineheight))  
}

geom_visualise.text <- function(geom, data = list()) {
  defaults <- list(x = 0.5, y = 0.5, angle = 45, label = "text")
  data <- modifyList(defaults, data)

  geom_grob(geom, data, default.units = "npc")
}
