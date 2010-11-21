context("Geoms")

geoms <- c("bar", "line", "path", "point", "polygon", "rect", "ribbon", "rug", "segment", "step", "text")

make_geom <- function(geom) {
  match.fun(paste("geom", geom, sep ="_"))()
}


test_that("Visualise method works for all geoms", {
  for(geom in geoms) {
    g <- make_geom(geom)
    expect_that(geom_visualise(g), is_a("grob"), label = geom_name(g)) 
  }
  
  
})