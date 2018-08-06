
test_that("assert handles custom errors", {

  expect_error(assert(FALSE), "is not")

  expect_error(assert(FALSE, "blah"), "blah")

  blah_error <- structure(
    class = c("blah_error", "error", "condition"),
    list(message = "blubb", call = call)
  )

  expect_silent(expect_error(assert(FALSE, blah_error)))

  expect_error(
    assert(FALSE, blah_error),
    class = "blah_error"
  )

})
