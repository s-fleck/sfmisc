context("utils-sfmisc")




# utils -------------------------------------------------------------------

test_that("prtunc works as expected", {

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




# assertions --------------------------------------------------------------

test_that("assert_namespace works as expected", {
  expect_error(
    assert_namespace("blubb", "schwupp"),
    'packages.*them.*install\\.packages\\(c\\("blubb", "schwupp"\\)\\)'
  )

  expect_error(
    assert_namespace("stats", "blubb"),
    'package.*it.*install\\.packages\\("blubb"\\)'
  )

  expect_error(
    assert_namespace("stats", c("blubb", "schwupp")),
    'package.*it.*c(.*blubb.*, .*schwupp.*)'
  )

})







# predicates --------------------------------------------------------------

test_that("is_POSIXct and co work as expected", {

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




test_that("is_windows_path works as expected", {
  expect_identical(
    is_windows_path(c(
      "d:",
      "C:\\Program Files",
      "c:\\Program Files",
      "c",
      "/home/foobar",
      "/")
    ),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})




# misc --------------------------------------------------------------------

test_that("path_tidy works as expected", {
  expect_identical(
    path_tidy(c(
      "c:\\Program Files",
      "~/rpkgs",
      "~/rpkgs/",
      "//foo/bar/",
      "//foo///bar/",
      "c:",
      "/"
    )),
    c(
      "C:/Program Files",
      "~/rpkgs",
      "~/rpkgs",
      "//foo/bar",
      "//foo/bar",
      "C:/",
      "/"
    )
  )
})



test_that("dupes works as expected", {

  expect_identical(dupes(c(1, 1, 1, 2, 3)), 1)

  drow <- cars[2, ]
  rownames(drow) <- NULL
  expect_identical(
    dupes(rbind(drow, drow, cars[1:3, ])),
    drow
  )
})
