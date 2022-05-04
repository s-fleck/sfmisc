#' Use sfmisc functions in current package
#'
#' @return `TRUE` on success
#' @export
#'
use_sfmisc <- function(){
  stopifnot(requireNamespace("rprojroot"))

  get_sfmisc_version <- function(x){
    txt <- readLines(x)

    if (length(txt) > 0){
      regmatches(txt[[1]], regexpr("(\\d\\.\\d\\.\\d$)|(\\d\\.\\d\\.\\d\\.\\d*$)", txt[[1]]))
    } else {
      "*.*.*"
    }
  }

  infile <- system.file("utils", "utils-sfmisc.R", package = "sfmisc")
  stopifnot(file.exists(infile))
  outfile  <- rprojroot::find_package_root_file("R", "utils-sfmisc.R")
  current_version <- utils::packageVersion("sfmisc")

  if (file.exists(outfile)){
    message(sprintf(
      "Replacing existing utils-sfmisc (v%s) with new version (v%s) at %s",
      get_sfmisc_version(outfile),
      current_version,
      outfile
    ))
    unlink(outfile)
  } else {
    message(sprintf(
      "Saving utils-sfmisc (v%s) to '%s'",
      current_version,
      outfile
    ))

  }

  code <- readLines(infile)
  code <- c(sprintf("# sfmisc utils %s\n\n", current_version), code)

  if (getOption("sfmisc.InlcudeDocs", FALSE)){
    code <- gsub("\\s*@noRd", "", code)
  }

  writeLines(code, outfile, useBytes = TRUE)
}
