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




# is_dir ------------------------------------------------------------------

test_that("dupes works as expected", {
  td <- file.path(tempdir(), "sfmisc-tests")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  p <- file.path(td, "emtpy")
  dir.create(p)
  expect_true(is_dir(p))
  expect_true(is_empty_dir(p))
  unlink(p, recursive = TRUE)
  expect_false(is_dir(p))

  d1 <- file.path(td, "not-empty-1")
  d2 <- file.path(td, "not-empty-1", "not-empty-2")
  f  <- file.path(td, "not-empty-1", "not-empty-2", "foo")

  dir.create(d1)
  dir.create(d2)
  file.create(f)

  # directories that contain either a file or a directory are not empty
  expect_false(is_empty_dir(d1))
  expect_false(is_empty_dir(d2))
  expect_false(is_dir(f))
  unlink(f)

  # empty directoris are empty
  expect_false(is_empty_dir(d1))
  expect_true(is_empty_dir(d2))
  unlink(d2, recursive = TRUE)

  # non-existing paths returns FALSE
  expect_true(is_empty_dir(d1))
  expect_false(is_empty_dir(d2))

  # cleanup
  unlink(d1, recursive = TRUE)
  expect_true(is_empty_dir(td))
})



test_that("dupes works as expected", {

  expect_true(is_candidate_key(c(1, 2, 3)))
  expect_true(is_candidate_key(1))
  expect_false(is_candidate_key(NA))

  expect_true(is_candidate_key(data.frame(a = c(1, 2, 3))))
  expect_true(is_candidate_key(data.frame(a = c(1))))
  expect_false(is_candidate_key(data.frame(a = NA)))
})




test_that("camelCase works as expected", {

  pascalCaseStrings <- c(
    "PascalCaseTestString",
    "PascalCaseTestString2",
    "PascalCase_TestString3")

  expect_identical(
    camelCase(pascalCaseStrings),
    c("pascalCaseTestString", "pascalCaseTestString2", "pascalCaseTestString3"))


  snake_case_test_strings <- c(
    "snake_case_test_string",
    "_snake_case_test_string_2_")

  expect_identical(
    camelCase(snake_case_test_strings),
    c("snakeCaseTestString", "snakeCaseTestString2"))


  spaced_test_strings <- c(
    "spaced test string",
    "spaced    test  string \n 2")

  expect_identical(
    camelCase(spaced_test_strings),
    c("spacedTestString", "spacedTestString2"))


  mixed_case_test_strings <- c(
    "mixed caseTest_String",
    "mixed CaseTest_String2")

  expect_identical(
    camelCase(mixed_case_test_strings),
    c("mixedCaseTestString", "mixedCaseTestString2"))

})


