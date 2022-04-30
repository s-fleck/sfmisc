pkgload::load_all()
sfmisc::use_sfmisc()

# remove @noRd tags and build site
utils_file <- here::here("R/utils-sfmisc.R")
code <- readLines(utils_file)
code <- gsub("@noRd", "", code)
writeLines(code, utils_file, useBytes = TRUE)
pkgload::load_all()
devtools::document()
pkgdown::build_site()

# reset utils-sfmisc
sfmisc::use_sfmisc()
