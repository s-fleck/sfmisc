#' Simple data validation
#'
#' **EXPERIMENTAL** Minimalistic mutli-criteria data validation. Designed to
#' work nicely together with `assert_all()` (see examples)
#'
#' @param ... Arbitrary expressions that evaluate to either `TRUE` or `FALSE`.
#'   Expressions that evaluate to anything else, will be counted as `FALSE` and
#'   throw a warning. You can name the expressions to generate nice labels
#'  (but that's usually not necessary).
#' @param .all `logical` scalar. Wrap each expression in `...` in `all()`
#'   (useful for validating columns in `data.frames`)
#'
#' @return a named `logical` vector that is guranteed to have no `NAs`.
#'
#'
#' @examples
#' validation <- validate(
#'   all(iris$Petal.Length < 5),
#'   `all species are valid` = all(iris$Species %in% c("setosa", "versicolor", "virginica")),
#'   `all sepals are small` = all(iris$Sepal.Length < 1)
#' )
#'
#' # validate can be used `with()` for added convenience:
#' validation <- with(iris, validate(
#'   all(Petal.Length < 5),
#'   `all species are valid` = all(Species %in% c("setosa", "versicolor", "virginica")),
#'   `all sepals are small` = all(Sepal.Length < 1)
#' ))
#'
#' # validate works together with assert_all to produce nice errors
#' try(
#'   assert_all(validation)
#' )
validate <- function(
    ...,
    .all = TRUE
){
  stopifnot(isTRUE(.all) || isFALSE(.all))
  expressions <- eval(substitute(alist(...)))

  result <- logical(length(expressions))
  criteria  <- character(length(expressions))
  criteria_names <- names(expressions)
  if (is.null(criteria_names)){
    criteria_names <- character(length(expressions))
  }


  for (i in seq_along(expressions)){
    criteria[[i]] <- paste(deparse(expressions[[i]], width.cutoff = 500L), collapse = " ")

    result[[i]] <-  tryCatch({
      res <- eval(expressions[[i]], envir = parent.frame(), enclos = parent.frame())
      if (.all){
        res <- all(res)
      }
      if (!is_scalar_bool(res)){
        stop("Cannot validate expression ", i, ": `", criteria[[i]], "` does not evaluate to either `TRUE` or `FALSE`", call. = FALSE)
      }
      res
    },

    error = function(e){
      warning(e$message, call. = FALSE)
      FALSE
    })
  }

  names(result) <- ifelse(is_blank(criteria_names), criteria, criteria_names)
  result
}




#' Assert all elements of a vector or list are `TRUE`
#'
#' Check if all elements of `x` are `TRUE`, and throw an informative warning that
#' contains the position and name (if any) of the element
#'
#' @param ... `logical` vectors or `lists` with only `logical` elements
#' @inheritParams assert
#'
#' @return `TRUE`
#' @seealso [assert()]
#'
#'
#' @examples
#' try(
#'   assert_all(c("this is true" = TRUE, "this is FALSE" = FALSE, FALSE))
#'   assert_all(
#'     list(a = c(TRUE, FALSE), b = TRUE),
#'     list(TRUE)
#'   )
#' )
assert_all <- function(
    ...,
    call = sys.call(-1)
){

  conds <- unlist(list(...))

  if (all(conds)){
    return(TRUE)
  }

  element_names <- names(conds)
  if (is.null(element_names)){
    element_names <- character(length(conds))
  }

  warning_messages <- character(0)

  for (i in seq_along(conds)){
    if (!isTRUE(conds[[i]])){
      if (is_blank(element_names[[i]])){
        warning_messages <- c(
          warning_messages,
          paste0("[", i, "] is not TRUE"))

      } else {
        warning_messages <- c(
          warning_messages,
          paste0("[", i, "] `", element_names[[i]], "` is not TRUE"))
      }
    }
  }

  error <- structure(
    list(
      message = paste0(c("Assertion failed", warning_messages), collapse = "\n"),
      call = call
    ),
    class = c("assertError", "error", "condition")
  )

  stop(error)
}
