#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
use_sfmisc <- function(x){
  assert_namespace("rprojroot")

  infile <- system.file("utils", "utils-sfmisc.R", package = "sfmisc")
  stopifnot(file.exists(infile))
  outfile  <- rprojroot::find_package_root_file("R", "utils-sfmisc.R")

  unlink(outfile)

  write(
    sprintf("# sfmisc utils %s\n\n\n\n", packageVersion("sfmisc")),
    outfile
  )


  file.append(outfile, infile)
}
