# sfmisc

Internal utility functions that I use across many of my packages, but that 
don't warant an external dependency. `sfmisc::use_sfmisc()` copies / updates 
those utility functions to the target R package in development. All functions 
in sfmisc depend only on base R.

This package is similar in fashion to 
[freebase](https://github.com/hrbrmstr/freebase) which is aimed at a wider
user base.

## Installation

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)


``` r
# install.packages("devtools")
devtools::install_github("s-fleck/sfmisc")
```
