context("utils-sfmisc")


test_that("utils-sfmisc works as expected", {

  x <- "racecar carrace"

  expect_identical(
    ptrunc(x, width = 99),
    "racecar carrace"
  )

  expect_identical(
    ptrunc(x, width = 8),
    "race ..."
  )

  expect_identical(
    ptrunc(x, width = nchar(x), collapse = NULL),
    x
  )
})



test_that("assert_namespace works as expected", {
  expect_error(
    assert_namespace("blubb", "schwupp"),
    'packages.*them.*install\\.packages\\(c\\("blubb", "schwupp"\\)\\)'
  )

  expect_error(
    assert_namespace("blubb"),
    'package.*it.*install\\.packages\\("blubb"\\)'
  )
})
