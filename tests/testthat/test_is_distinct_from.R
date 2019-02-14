context("is_distinct_from")

test_that("is_distinct_from", {

  expect_true(is_distinct_from(1, 2))
  expect_false(is_not_distinct_from(1, 2))

  expect_true(is_distinct_from(1, NA))
  expect_false(is_not_distinct_from(1, NA))

  expect_false(is_distinct_from(NA, NA))
  expect_true(is_not_distinct_from(NA, NA))
})

