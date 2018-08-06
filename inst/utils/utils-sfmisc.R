# utils -------------------------------------------------------------------

compact <- function(x){
  x[!vapply(x, is.null, FALSE)]
}




walk <- function(.x, .f, ...){
  for (i in seq_along(.x)){
    .f(.x[[i]], ...)
  }

  invisible(.x)
}




# assertions --------------------------------------------------------------

#' Assert a condition
#'
#' A simpler and more efficient for [base::stopifnot()] that has an easy
#' mechanism for supplying custom error messages. As opposed to `stopifnot()`,
#' `assert()` only works with a single (scalar) assertions.
#'
#' @param cond `TRUE` or `FALSE` (without any attributes). `FALSE` will throw
#'   an exception with an automatically constructed error message (if `...`
#'   was not supplied). Anything else will throw an exception stating that
#'   `cond` was not valid.
#' @param ... passed on to [stop()]
#' @param call. passed on to [stop()]
#' @param domain passed on to [stop()]
#'
#' @noRd
#'
#' @return TRUE on success
#'
#' @examples
#'
#' \dontrun{
#' assert(1 == 1)
#' assert(1 == 2)
#' }
#'
#'
assert <- function(
  cond,
  ...,
  call. = FALSE,
  domain = NULL
){
  if (identical(cond, TRUE)){
    return(TRUE)
  } else if (identical(cond, FALSE)){
    if (identical(length(list(...)), 0L)){
      msg <- paste0("`", deparse(match.call()[[2]]), "`", " is not 'TRUE'")
      stop(msg, call. = call., domain = domain)
    } else {
      suppressWarnings( stop(..., call. = call., domain = domain) )
    }

  } else {
    stop("Assertion must be either 'TRUE' or 'FALSE'")
  }
}




assert_namespace <- function(x){
  assert(requireNamespace(x, quietly = TRUE))
  invisible(TRUE)
}




# conditions --------------------------------------------------------------

#' Condition constructor
#'
#' A constructur function for conditions, taken from
#' \url{http://adv-r.had.co.nz/beyond-exception-handling.html}
#'
#' @param subclass Subclass to assign to the condition
#' @param message  message to be passed to the condition
#' @param call     call passed on to the conditon
#' @param ...      further list elements to be passed on to the resulting object
#'
#' @return a condition object
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' # Construct a custom condition
#' malformed_log_entry_error <- function(text) {
#'   msg <- paste0("Malformed log entry: ", text)
#'   condition(
#'     c("malformed_log_entry_entry", "error"),
#'     message = msg,
#'     text = text
#'   )
#' }
#'
#'
#' # Signal the condition
#' parse_log_entry <- function(text) {
#'   if (!well_formed_log_entry(text)) {
#'     stop(malformed_log_entry_error(text))
#'    }
#' }
#'
#'
#' # Handle the condition
#' tryCatch(
#'   malformed_log_entry = function(e) NULL,
#'   parse_log_entry(text)
#' )
#' }
#'
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}




error <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "error", "condition"),
    list(message = message, call = call, ...)
  )
}




# predicates --------------------------------------------------------------
is_scalar <- function(x){
  identical(length(x), 1L)
}




is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}




is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




is_tf <- function(x){
  is.logical(x) && !anyNA(x)
}




is_scalar_tf <- function(x){
  identical(x, TRUE) || identical(x, FALSE)
}




is_integerish <- function(x){
  if (!is.numeric(x)){
    FALSE
  } else {
    all(as.integer(x) == x)
  }
}




is_equal_length <- function(...){
  lengths <- vapply(list(...), length, 1L)
  identical(length(unique(lengths)), 1L)
}




is_empty <- function(x){
  identical(length(x), 0L)
}




is_blank <- function(x){
  trimws(x) == ""
}




# all_are -----------------------------------------------------------------


#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by [unique()] (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0 (e.g. `NULL`, `numeric()`, ...)
#'
#' @noRd
#' @family special equality checks
#' @return `TRUE/FALSE`
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_identical <- function(x, empty_value = FALSE) {
  assert(length(empty_value) <= 1)

  if (length(x) > 0L) {
    return(identical(length(unique(x)), 1L))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_are_identical
#'
#' @return TRUE/FALSE
#'
#' @noRd
#' @family special equality checks
#'
#' @examples
#'
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
#'
all_are_distinct <- function(
  x,
  empty_value = FALSE
){
  assert(length(empty_value) <= 1)

  if (identical(length(x), 1L)) {
    return(TRUE)

  } else if (length(x) > 1L) {
    return(identical(length(unique(x)), length(x)))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




# case_when ---------------------------------------------------------------

#' A general vectorised if
#'
#' This function allows you to vectorise multiple `if` and `else if`
#' statements. It is an R equivalent of the SQL `CASE WHEN` statement.
#'
#' @param ... A sequence of two-sided formulas. The left hand side (LHS)
#'   determines which values match this case. The right hand side (RHS)
#'   provides the replacement value.
#'
#'   The LHS must evaluate to a logical vector. The RHS does not need to be
#'   logical, but all RHSs must evaluate to the same type of vector.
#'
#'   Both LHS and RHS may have the same length of either 1 or `n`. The
#'   value of `n` must be consistent across all cases. The case of
#'   `n == 0` is treated as a variant of `n != 1`.
#'
#' @return A vector of length 1 or `n`, matching the length of the logical
#'   input or output vectors, with the type (and attributes) of the first
#'   RHS. Inconsistent lengths or types will generate an error.
#'
#' @examples
#' x <- 1:50
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#'
#' # Like an if statement, the arguments are evaluated in order, so you must
#' # proceed from the most specific to the most general. This won't work:
#' case_when(
#'   TRUE ~ as.character(x),
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz"
#' )
#'
#' # All RHS values need to be of the same type. Inconsistent types will throw an error.
#' # This applies also to NA values used in RHS: NA is logical, use
#' # typed values like NA_real_, NA_complex, NA_character_, NA_integer_ as appropriate.
#' case_when(
#'   x %% 35 == 0 ~ NA_character_,
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA_real_
#' )
#' # This throws an error as NA is logical not numeric
#' \dontrun{
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA
#' )
#' }
#'
#' dat <- iris[1:5, ]
#' dat$size<- case_when(
#'   dat$Sepal.Length < 5.0 ~ "small",
#'   TRUE ~ "big"
#' )
#' dat

case_when <- function(...) {
  formulas <- list(...)
  n <- length(formulas)

  if (n == 0) {
    stop("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (!inherits(f, "formula") || length(f) != 3) {
      stop(sprintf(
        "Case %s (`%s`) must be a two-sided formula, not a %s",
        i,
        deparse_trunc(substitute(list(...))[[i + 1]]),
        typeof(f)
      ))
    }

    env <- environment(f)
    query[[i]] <- eval(f[[2]], env)

    if (!is.logical(query[[i]])) {
      stop(sprintf(
        "LHS of case %s (%s) must be a logical, not %s",
        i,
        backticks(deparse_trunc(f_lhs(f))),
        typeof(query[[i]])
      ))
    }

    value[[i]] <- eval(f[[3]], env)
  }

  lhs_lengths <- vapply(query, length, integer(1))
  rhs_lengths <- vapply(value, length, integer(1))
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))

  if (length(all_lengths) <= 1) {
    m <- all_lengths[[1]]
  } else {
    non_atomic_lengths <- all_lengths[all_lengths != 1]
    m <- non_atomic_lengths[[1]]
    if (length(non_atomic_lengths) > 1) {
      inconsistent_lengths <- non_atomic_lengths[-1]
      lhs_problems <- lhs_lengths %in% inconsistent_lengths
      rhs_problems <- rhs_lengths %in% inconsistent_lengths
      problems <- lhs_problems | rhs_problems
      bad_calls(
        formulas[problems],
        check_length_val(inconsistent_lengths, m, header = NULL, .stop = identity)
      )
    }
  }

  out <- value[[1]][rep(NA_integer_, m)]
  replaced <- rep(FALSE, m)

  for (i in seq_len(n)) {
    out <- replace_with(out, query[[i]] & !replaced, value[[i]], NULL)
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }

  out
}




#' Vectorised if
#'
#' Compared to the base [ifelse()], this function is more strict.
#' It checks that `true` and `false` are the same type. This
#' strictness makes the output type more predictable, and makes it somewhat
#' faster.
#'
#' @param condition Logical vector
#' @param true,false Values to use for `TRUE` and `FALSE` values of
#'   `condition`. They must be either the same length as `condition`,
#'   or length 1. They must also be the same type: `if_else()` checks that
#'   they have the same type and same class. All other attributes are
#'   taken from `true`.
#' @param missing If not `NULL`, will be used to replace missing
#'   values.
#' @return Where `condition` is `TRUE`, the matching value from
#'   `true`, where it's `FALSE`, the matching value from `false`,
#'   otherwise `NA`.
#'
#' @examples
#' x <- c(-5:5, NA)
#' if_else(x < 0, NA_integer_, x)
#' if_else(x < 0, "negative", "positive", "missing")
#'
#' # Unlike ifelse, if_else preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#' if_else(x %in% c("a", "b", "c"), x, factor(NA))
#' # Attributes are taken from the `true` vector,
if_else <- function(
  condition,
  true,
  false,
  missing = NULL
){
  if (!is.logical(condition)) {
    stop("`condition` must be a logical, not ", typeof(condition))
  }

  out <- true[rep(NA_integer_, length(condition))]
  out <- replace_with(
    out,
    condition,
    true,
    "`true`",
    sprintf("length of `condition`")
  )

  out <- replace_with(
    out,
    !condition,
    false,
    "`false`",
    sprintf("length of `condition`")
  )

  out <- replace_with(
    out,
    is.na(condition),
    missing,
    "`missing`",
    sprintf("length of `condition`")
  )

  out
}




# case_when helpers -------------------------------------------------------




backticks <- function (x){
  paste0("`", x, "`")
}




deparse_trunc <- function(x, width = getOption("width")){
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width)
    return(text)
  paste0(substr(text[1], 1, width - 3), "...")
}




f_lhs <- function(x) x[[2]]




f_rhs <- function(x) x[[3]]




bad_calls <- function(calls, ...){
  stop(fmt_calls(calls), " ", ...)
}




fmt_calls <- function(...){
  paste(backticks(vapply(..., deparse, "")), collapse = ", ")
}




check_length_val <- function(
  length_x,
  n,
  header,
  reason = NULL,
  .stop = stop
){
  if (all(length_x %in% c(1L, n)))
    return()

  if (is.null(reason))
    reason <- ""
  else
    reason <- paste0(" (", reason, ")")

  if (is.null(header))
    header <- ""
  else
    header <- paste0(header, " ")


  if (n == 1) {
    .stop(sprintf("%smust be length 1%s, not %s", header, reason, paste(length_x, collapse = ", ")))
  } else {
    .stop(sprintf("%smust be length %s%s or one, not %s", header, n, reason, paste(length_x, collapse = ", ")))
  }
}




replace_with <- function (
  x,
  i,
  val,
  name,
  reason = NULL
){
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason)
  check_type(val, x, name)
  check_class(val, x, name)

  i[is.na(i)] <- FALSE
  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }
  x
}




check_length <- function (
  x,
  template,
  header,
  reason = NULL
){
  check_length_val(length(x), length(template), header, reason)
}




check_type <- function(
  x,
  template,
  header
){
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  if (is.null(header))
    header <- ""
  else
    header <- paste0(header, " ")

  stop(sprintf("%smust be type %s, not %s", header, typeof(template), typeof(x)))
}




check_class <- function(
  x,
  template,
  header
){
  if (!is.object(x)) {
    return()

  } else if (identical(class(x), class(template))) {
    return()

  } else {

    if (is.null(header))
      header <- ""
    else
      header <- paste0(header, " ")


    stop(sprintf("%smust be type %s, not %s", header, typeof(template), typeof(x)))
  }
}




fmt_classes <- function(
  x
){
  paste(class(x), collapse = "/")
}
