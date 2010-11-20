#' Marginal rug plots
#'
#' @export
#' @S3method aes_required geom
#' @S3method geom_grob rug
#' @param length length of rug tassles, as a grid unit
geom_rug <- function(aesthetics = list(), length = unit(0.03, "npc")) {
  geom_from_call(c("rug", "path"))
}

aes_required.rug <- function(geom) c()

geom_grob.rug <- function(geom, data) {
  rugs <- list()
  if (!is.null(data$x)) {
    rugs$x <- segmentsGrob(
      x0 = unit(data$x, "native"), x1 = unit(data$x, "native"), 
      y0 = unit(0, "npc"), y1 = geom$length, 
      gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, 
        lwd = data$size * .pt)
    )
  }  

  if (!is.null(data$y)) {
    rugs$y <- segmentsGrob(
      y0 = unit(data$y, "native"), y1 = unit(data$y, "native"), 
      x0 = unit(0, "npc"), x1 = geom$length,
      gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, 
        lwd = data$size * .pt)
    )
  }  
  
  grobTree(rugs$x, rugs$y)
}
