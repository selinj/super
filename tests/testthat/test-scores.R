context("Supervised cluster scores")

test_that("Completeness is correct", {

  a <- c(0, 0, 1, 1)
  b <- c(1, 1, 0, 0)
  c <- c(0, 0, 0, 0)
  d <- c(0, 1, 2, 3)
  e <- c(0, 1, 0, 1)

  expect_equal(completeness(table(a, b)), 1)
  expect_equal(completeness(table(a, c)), 1)
  expect_equal(completeness(table(d, a)), 1)
  expect_equal(completeness(table(a, e)), 0)
  expect_equal(completeness(table(c, d)), 0)

})


test_that("Homogeneity is correct", {

  a <- c(0, 0, 1, 1)
  b <- c(1, 1, 0, 0)
  c <- c(0, 0, 0, 0)
  d <- c(0, 0, 1, 2)
  e <- c(0, 1, 2, 3)
  f <- c(0, 0, 0, 0)

  expect_equal(homogeneity(table(a, b)), 1)
  expect_equal(homogeneity(table(a, d)), 1)
  expect_equal(homogeneity(table(a, e)), 1)
  expect_equal(homogeneity(table(a, c)), 0)
  expect_equal(homogeneity(table(a, f)), 0)

})
