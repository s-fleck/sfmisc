context("equalish")


test_that("is_equalish works as expected", {
  a <- 0.58
  b <- 0.08

  expect_false(a - b == 0.5)
  expect_true(equalish(a - b, 0.5))
  expect_false(equalish(a - b, -0.5))
  expect_true(equalish(-(a - b), -0.5))
  expect_false(equalish(a - b, 0.5001))
})




test_that("equalish_frac works as expected", {

  a <- 10
  b <- 11

  expect_true(equalish_frac(a, b, tolerance = 0.1))
  expect_true(equalish_frac(-a, -b, tolerance = 0.1))
  expect_false(equalish_frac(a, -b, tolerance = 0.1))
  expect_false(equalish_frac(-a, b, tolerance = 0.1))
  expect_false(equalish_frac(a, b + 1, tolerance = 0.1))
  expect_false(equalish_frac(a -1 , b, tolerance = 0.1))

  expect_true(all(equalish_frac(
    c(0, 2, 3),
    c(0, 2.1, 3.1),
    tolerance = 0.1
  )))


  expect_equal(
    equalish_frac(c(10, 11, NA), c(10, 8,  NA)),
    c(TRUE, FALSE, NA)
  )
})
