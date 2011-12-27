context("Order list")

test_that("order_list leaves single elements unchanged", {
  x1 <- list(a = 1, b = 1, c = 1)
  x2 <- order_list(x1, names(x1))
  
  expect_equal(x1, x2)
  
  x3 <- list(a = 10:1, b = 1, c = 1)
  x4 <- order_list(x3, "a")
  expect_equal(x4$a, 1:10)
  expect_equal(x3$b, x4$b)
  expect_equal(x3$c, x4$c)
  
})

test_that("order_list orders all columns", {
  x1 <- list(a = 10:1, b = 1:10, c = 1:10)
  x2 <- order_list(x1, "a")
  
  expect_equal(x2$a, 1:10)
  expect_equal(x2$b, 10:1)
  expect_equal(x2$c, 10:1)
})