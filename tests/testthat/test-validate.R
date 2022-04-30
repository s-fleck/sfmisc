test_that("validate works with unnamed expressions", {

  res <- validate(
    all(iris$Petal.Length == 5),
    all(iris$Species %in% iris$Species)
  )

  expect_true(all(res == c(FALSE, TRUE)))
  expect_identical(names(res), c("all(iris$Petal.Length == 5)", "all(iris$Species %in% iris$Species)"))
})




test_that("validate works with named expressions", {

  res <- validate(
    Petals = all(iris$Petal.Length == 5),
    Species = all(iris$Species %in% iris$Species)
  )

  expect_true(all(res == c(FALSE, TRUE)))
  expect_identical(names(res), c("Petals", "Species"))
})




test_that("validate works with named and unnamed expressions", {

  x <- validate(
    all(iris$Petal.Length == 5),
    Species = all(iris$Species %in% iris$Species)
  )

  expect_true(all(x == c(FALSE, TRUE)))
  expect_identical(names(x), c("all(iris$Petal.Length == 5)", "Species"))
})




test_that("validate works within `with()`", {

  x <- iris

  res <-
    with(x, validate(
      all(Petal.Length == 5),
      Species = all(iris$Species %in% iris$Species)
    )
  )

  expect_true(all(res == c(FALSE, TRUE)))
  expect_identical(names(res), c("all(Petal.Length == 5)", "Species"))
})




test_that("validate warns if an expressions evaluates to non-bool", {

  res <- expect_warning(validate(
    Petals = all(iris$Petal.Length == 5),
    Species = iris$Species %in% iris$Species
  ), regexp = ".*Species.*does not evaluate")

  expect_true(!any(res))
  expect_identical(names(res), c("Petals", "Species"))
})
