# sfmisc

Utility functions that I use across many of my packages, but that 
don't warrant an external dependency. All functions in sfmisc depend only on
base-R.

sfmisc can be loaded like any normal R package, but it can also be  
embedded into other R packages with `sfmisc::use_sfmisc()`. Since all functions
are small and self-contained, this is a convenient way to get some extra
utilities into an R-package without adding an external dependency. 

**Note:** Many (but not all) of the helpers included in this package overlap
with tidyverse functions of the same name (but without compilation dependencies)


## Installation

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)


``` r
# install.packages("devtools")
devtools::install_github("s-fleck/sfmisc")
```
