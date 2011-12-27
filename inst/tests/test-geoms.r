context("Geoms")

geoms <- c("bar", "line", "path", "point", "polygon", "rect", "ribbon", "rug", "segment", "step", "text")

make_geom <- function(geom) {
  match.fun(paste("geom", geom, sep ="_"))()
}


test_that("Visualise method works for all geoms", {
  for(geom in geoms) {
    g <- make_geom(geom)
    
    grob <- geom_visualise(g)
    name <- geom_name(g)
    expect_that(grob, is_a("grob"), label = name) 
    
    png(paste("visual/icon-", name, ".png", sep = ""), 
      width = 100, height = 100)
    grid.newpage()
    pushViewport(viewport())  
    grid.draw(grob)
    dev.off()
  }
  
  
})