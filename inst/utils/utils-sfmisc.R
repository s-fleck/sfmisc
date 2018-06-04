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

assert_namespace <- function(x){
  stopifnot(requireNamespace(x, quietly = TRUE))
}





# predicates --------------------------------------------------------------

is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




is_scalar <- function(x){
  identical(length(x), 1L)
}




is_flag <- function(x){
  is.scalar(x) & is.logical(x)
}




is_empty <- function(x){
  identical(length(x), 0L)
}




is_blank <- function(x){
  trimws(x) == ""
}

