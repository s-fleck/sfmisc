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





test_that("assert_namespace works as expected", {

  expect_true(is_POSIXct(Sys.time()))
  expect_true(is_scalar_POSIXct(Sys.time()))
  expect_true(is_scalar_POSIXt(Sys.time()))
  expect_false(is_scalar_POSIXlt(Sys.time()))

  lt <- as.POSIXlt(Sys.time())
  expect_true(is_POSIXlt(c(lt, lt)))
  expect_true(is_scalar_POSIXlt(lt))
  expect_true(is_scalar_POSIXt(lt))
  expect_false(is_scalar_POSIXct(lt))

  expect_true(is_Date(Sys.Date()))
  expect_true(is_scalar_Date(Sys.Date()))

})
