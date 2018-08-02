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
#'   These dots support [tidy dots][rlang::tidy-dots] features.
#' @export
#' @return A vector of length 1 or `n`, matching the length of the logical
#'   input or output vectors, with the type (and attributes) of the first
#'   RHS. Inconsistent lengths or types will generate an error.
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
#' # case_when is particularly useful inside mutate when you want to
#' # create a new variable that relies on a complex combination of existing
#' # variables
#' starwars %>%
#'   select(name:mass, gender, species) %>%
#'   mutate(
#'     type = case_when(
#'       height > 200 | mass > 200 ~ "large",
#'       species == "Droid"        ~ "robot",
#'       TRUE                      ~  "other"
#'     )
#'   )
#'
#' # Dots support splicing:
#' patterns <- list(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#' case_when(!!!patterns)
#'
#'

case_when <- function(...) {
  case_when_list(list(...))
}




case_when_list <- function(formulas) {
  assert(is.list(formulas))

  n <- length(formulas)

  if (n == 0) {
    stop("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (!inherits(f, "formula") || length(f) != 3) {
      non_formula_arg <- substitute(formulas)[[i + 1]]
      header <- sprintf("Case %s (%s)", i,  fmt_obj1(deparse_trunc(non_formula_arg)))
      stop(header, " must be a two-sided formula, not a ", typeof(f))
    }

    env <- environment(f)

    query[[i]] <- eval(f[[2]], env)
    if (!is.logical(query[[i]])) {
      header <- sprintf("LHS of case %s (%s)", i, fmt_obj1(deparse_trunc(f_lhs(f))))
      stop(header, " must be a logical, not ", typeof(query[[i]]))
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
        check_length_val(inconsistent_lengths, m, header = NULL)
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



fmt_obj1 <- function (x){
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


bad_calls <- function(calls, ..., .envir = parent.frame()){
  stop(fmt_calls(calls), ..., .envir = .envir)
}


fmt_calls <- function(...){
  x <- vapply(..., deparse, "")
}



check_length_val <- function (length_x, n, header, reason = NULL){


  if (all(length_x %in% c(1L, n))) {
    return()
  }
  if (is.null(reason))
    reason <- ""
  else
    reason <- paste0(" (", reason, ")")
  if (n == 1) {
    stop(sprintf("must be length 1%s, not %s", reason, paste(length_x, collapse = ", ")))
  } else {
    stop(sprintf("must be length %s%s or one, not %s", n, reason, paste(length_x, collapse = ", ")))
  }
}



replace_with <- function (x, i, val, name, reason = NULL)
{
  if (is.null(val)) {
    return(x)
  }
  check_length(val, x, name, reason)
  check_type(val, x, name)
  check_class(val, x, name)
  i[is.na(i)] <- FALSE
  if (length(val) == 1L) {
    x[i] <- val
  }
  else {
    x[i] <- val[i]
  }
  x
}


check_length <- function (x, template, header, reason = NULL)
{
  check_length_val(length(x), length(template), header, reason)
}


check_type <- function (x, template, header)
{
  if (identical(typeof(x), typeof(template))) {
    return()
  }
  stop(header, sprintf("must be type %s, not %s", type_of(template), typeof(x)))
}



check_class <- function (x, template, header)
{
  if (!is.object(x)) {
    return()
  }
  if (identical(class(x), class(template))) {
    return()
  }
  stop(header, sprintf("must be type %s, not %s", type_of(template), typeof(x)))
  glubort(header, "must be %s, not %s", fmt_classes(template), fmt_classes(x))
}


fmt_classes <- function (x)
{
  paste(class(x), collapse = "/")
}
