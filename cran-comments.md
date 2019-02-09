## Release summary

This major update has incorporated the tidy evaluation principles from the recent tidyverse packages, which also fixes a bug that would have occurred with the upcoming release of dplyr 0.8.0. Soft-deprecated SE tidyverse function (SE versions) are no longer used to avoid future code breaks and variables in dataframes are accessed as unquoted symbols converted from strings using !!rlang::sym(). All aesthetic mappings are based on variables within dataframes instead single vectors as before.

Other changes include different statistics computations and added functionality through new standalone functions. Some bugs were also fixed.


## Test environments

* local OS X install, R 3.5.0
* local OS X install, R 3.5.2
* win-builder (devel and release)

## R CMD check results 

There were 0 errors, 0 warnings and 0 notes.



## Reverse dependencies

There are currently no downstream dependencies for this package.



