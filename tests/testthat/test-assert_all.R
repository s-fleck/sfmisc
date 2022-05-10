context("assert")

test_that("assert handles custom errors", {

  expect_error(assert_all(c(FALSE, FALSE, TRUE)), class = "assertError")
  expect_true(assert_all(c(TRUE, TRUE, TRUE)))

  expect_error(
    assert_all(
      list(a = c(TRUE, FALSE), b = TRUE),
      list(TRUE)
    ),
    class = "assertError"
  )

  expect_error(
    assert_all(
      list(a = c(TRUE, FALSE), b = TRUE),
      list(TRUE),
      list(d = FALSE)
    ),
    class = "assertError"
  )

  expect_warning(expect_error(
    assert_all(
      list(a = c(TRUE, FALSE), b = TRUE),
      list(TRUE),
      list(d = "FALSE")
    ),
    class = "assertError"
  ))
})
