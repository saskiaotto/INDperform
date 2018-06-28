# INDperform 0.1.0

* All functions now have data input validation routines that will return detailed messages if the required input has not the correct format. This prevents potential error messages when running following functions.

* In all modelling functions potential error messages that occurr as side effects in the model fitting are captured and printed out together with the model id, indicator and pressure variable or saved in the output tibble.

## Bug fixes

* `plot_spiecharts` now orders the pressure-specific slices correctly to the pressure types.

* All modelling functions can now handle all basic distribution families and some of the mgcv families.

* `expect_response` now returns the modified input tibble with the correct column names.

* In `model_gamm` the length of the outlier list to exclude (excl_outlier argument) is now correctly estimated in the data input validation routine.


# INDperform 0.1.0.9000
(soon released as v0.1.1)

* With the upcoming release of ggplot2 v2.3.0 we deactivated our visual tests to avoid conflicts between generated and references plots that would cause tests to fail.
