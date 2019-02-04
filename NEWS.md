# INDperform 0.1.0

* All functions now have data input validation routines that will return detailed messages if the required input has not the correct format. This prevents potential error messages when running following functions.

* In all modelling functions potential error messages that occurr as side effects in the model fitting are captured and printed out together with the model id, indicator and pressure variable or saved in the output tibble.

## Bug fixes

* `plot_spiecharts` now orders the pressure-specific slices correctly to the pressure types.

* All modelling functions can now handle all basic distribution families and some of the mgcv families.

* `expect_response` now returns the modified input tibble with the correct column names.

* In `model_gamm` the length of the outlier list to exclude (excl_outlier argument) is now correctly estimated in the data input validation routine.

# INDperform 0.1.1

* With the upcoming release of ggplot2 v2.3.0 we deactivated our visual tests to avoid conflicts between generated and references plots that would cause tests to fail.

* Minor modifications in the test files to pass all system checks on CRAN.

# INDperform 0.1.1.9000

* Fixed issue of missing acf and pacf diagnostic plots as soon as there were NAs in the acf vectors (happens if time series is very short).

* NRMSE computation in `model_gam()` and `model_gamm()` is now based on the standard deviation instead of the mean as before. This has consequences for the overall scale of the NRMSE, hence, the cut-off values for the scoring were adjusted in the criteria score template (`crit_scores_tmpl`): from > 0.4 (score 0), > 0.1 (score 1) and <= 0.1 (score 2) to > 2 (score 0), > 1 (score 1) and <= 1 (score 2).

* The actual function for computing the NRMSE is now avaialable as standalone function `nrmse()`; the function allows 4 different types of normalization and has as additional arguments for the specification of the type of transformation applied to the observations prior to the analysis. If the transformation is specified the function computes the NRMSE on the back-transformed observations and predictions, which is recommended for indicator cross-comparisons (see also [https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/](https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/).

* The internal `calc_nrmse()`has been rewritten so that it is a wrapper function of `nrmse()`. It now not only serves as internal helper function for `model_gam()` and `model_gamm()`, but can be used by the user to compute the NRMSE for all models using different settings than the default (i.e. using a different normalization method and allow partial back-transformations). The function takes as input the model list (e.g. `$model` in the final model tibble), a list of indicator values (e.g. the `$ind_test` vectors from the `ind_init()` function) and a list of pressure values (e.g. the `$press_test` vectors) to calculate first the predicted values given the model and pressure values, then -if specified- the back-transformation and finally the NRMSE for the individual models.

* The `summary_sc()` function has a new 3rd output list, which shows all the pressure-independent scores and the pressure-specific scores for both sensitivity and robustnest (i.e. the sum of C9 and C10 sub-criteria) as matrix. This table now serves as bases for some score-based IND performance functions (i.e. `dist_sc()`, `plot_spiechart()`).

* The `dist_sc()` takes now as input the new sub`$scores_matrix` from the `summary_sc()` function (instead of the output tibble from the `scoring()` function).

* The function `dist_sc_group()` was added, which allows the calculation of the distance matrix averaged across groups, hence, it is like a weighted distance matrix.
