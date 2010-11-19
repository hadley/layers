context("Aesthetics")

df <- data.frame(x = 1:10, y = 1:10)

test_that("data overrides defaults", {  
  aes <- calc_aesthetics(geom_point(), df)
  expect_equal(length(aes$x), 10)
  expect_equal(length(aes$y), 10)
  expect_equal(length(aes$colour), 1)
  expect_equal(length(aes$shape), 1)
})

test_that("parameters override defaults and data", {  
  aes <- calc_aesthetics(geom_point(list(x = 1, colour = "red")), df)

  expect_equal(aes$x, 1)
  expect_equal(aes$colour, "red")
})

test_that("missing aesthetics cause error", {
  expect_error(calc_aesthetics(geom_point(), data.frame()), 
    "missing aesthetics: x, y")
    
  df <- data.frame(x = 1)
  expect_error(calc_aesthetics(geom_point(list(x = NULL, y = 1)), df), 
    "missing aesthetics")
})

test_that("parameters work without data", {
  aes <- calc_aesthetics(geom_point(list(x = 1, y = 1)), df)
  expect_equal(aes$x, 1)
  expect_equal(aes$y, 1)
})