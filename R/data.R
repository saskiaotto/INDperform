#################### User template #####################

#' Template of (sub-)criteria and corresponding scoring information
#'
#' This table serves as basis for the \code{\link{scoring}} function and builds on
#' the criterion-scoring scheme described in the underlying framework (Otto \emph{et al.},
#' 2018). The user can modify the weights, scores, conditions or remove specific (sub)crits.
#'
#' @format A data frame with 27 rows and 12 variables:
#' \describe{
#' \item{crit_id}{ID of main criteria.}
#' \item{crit}{A vector of the main criteria.}
#' \item{subcrit_id}{ID of subcriteria.}
#' \item{subcrit}{A vector of the subcriteria.}
#' \item{definition}{A short description of the (sub)criteria.}
#' \item{score_explanation}{A short explanation on the required condition
#'           for a specific score.}
#' \item{score}{A vector of scores for each (sub-)criterion and the respective
#'           condition.}
#' \item{weight}{Weights assigned to each score to allow easy user adjustments.
#'           Default is 1 for all scores.}
#' \item{score_pressure_specific}{Additional information whether the scoring is
#'           pressure-specific or not.}
#' \item{condition}{A list-column single elements or vectors with various elements
#'           containing score-specific conditions put in R Syntax against which
#'           (sub-)criterion-specific variables (see condition_var) are checked.}
#' \item{condition_var}{The variables used as basis for scoring the specific
#'           (sub-)criterion.}
#' \item{func_name}{The names of the function that generate output tibbles
#'           containing the required variables.}
#' }
"crit_scores_tmpl"


#################### Example datasets #####################

#' Food web indicators of the Central Baltic Sea (Bornholm Basin).
#'
#' @format A data frame with 30 rows and 13 variables. These indicator time series
#' represent a slight modification of the original time series represented in
#' Otto et al. (2018).
#' \describe{
#' \item{Year}{year given as integer}
#' \item{TZA}{ln-transformed Total Zooplankton Abundance (in N/m^2)}
#' \item{MS}{Mean Size of zooplankton (in wet weight $\mu$g)}
#' \item{rCC}{ln-transformed ratio of Cladoceran to Copepod Abundance}
#' \item{Cops}{ln-transformed Copepod biomass (in mg)}
#' \item{Micro}{ln-transformed biomass of Microphageous zooplankton (in mg)}
#' \item{rZPPP}{ratio of Zooplankton to Phytoplankton biomass}
#' \item{Sprat}{ln-transformed Sprat abundance (in million N)}
#' \item{Herring}{ln-transformed Herring abundance (in millions N)}
#' \item{Stickle}{ln-transformed Stickleback CPUE (in kg h^-1^)}
#' \item{Cod}{ln-transformed Cod CPUE (in kg h^-1^)}
#' \item{SPF}{ln-transformed Small Predatory Fish (i.e. sprat and herring <10cm) (in million g)}
#' \item{LPF}{ln-transformed Large Predatory Fish (i.e. Cod >38cm) (in kg h^-1^)}
#' }
"ind_ex"


#' Environmental variables representing pressures for pelagic food webs in
#' the Central Baltic Sea (Bornholm Basin).
#'
#' @format A data frame with 30 rows and 8 variables. These indicator time series
#' represent a slight modification of the original time series represented in
#' Otto et al. (2018)
#' \describe{
#' \item{Year}{year given as integer}
#' \item{Tsum}{temperature summer (in °C)}
#' \item{Swin}{salinity winter}
#' \item{Pwin}{phosphate in winter (in mg/m3)}
#' \item{Nwin}{nitrogen winter (in mg/m3)}
#' \item{Fsprat}{fishing mortality of sprat}
#' \item{Fher}{fishing mortality of herring}
#' \item{Fcod}{fishing mortality of cod}
#' }
"press_ex"


#' Pressure variables and their associated pressure types from our example data
#'
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#' \item{press}{pressure name}
#' \item{press_type}{corresponding pressure type}
#' }
"press_type_ex"



############ Output tibbles from functions using the demonstration data #################


#' Model output tibble from the \code{\link{model_trend}} function
#'
#' This is an example output tibble from the \code{model_trend} function applied on the
#' Central Baltic Sea food web indicator demonstration data.
#'
#' @format A data frame with 12 rows and 9 variables:
#' \describe{
#'   \item{\code{ind_id}}{Indicator IDs.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{p_val}}{The p values for the smoothing term (here time).}
#'   \item{\code{model}}{A list-column of indicator-specific gam objects.}
#'   \item{\code{ind_train}}{A list-column with indicator values of the training data.}
#'   \item{\code{time_train}}{A list-column with the time values (e.g. years) of the
#'              training data.}
#'   \item{\code{pred}}{A list-column with indicator values predicted from the GAM
#'              for the training period.}
#'   \item{\code{ci_up}}{A list-column with the upper 95\% confidence interval of
#'              predicted indicator values.}
#'   \item{\code{ci_low}}{A list-column with the lower 95\% confidence interval of
#'              predicted indicator values.}
#' }
"model_trend_ex"

#' Output tibble from the \code{\link{ind_init}} function
#'
#' This is an example output tibble from the \code{ind_init} function applied
#' on the Central Baltic Sea food web indicator demonstration data.
#'
#' @format A data frame with 84 rows and 10 variables:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{ind_train}}{A list-column with indicator values of the training data.}
#'   \item{\code{press_train}}{A list-column with pressure values of the training data.}
#'   \item{\code{time_train}}{train data from year}
#'   \item{\code{ind_test}}{A list-column with indicator values of the test data.}
#'   \item{\code{press_test}}{A list-column with pressure values of the test data.}
#'   \item{\code{time_test}}{test data from year}
#'   \item{\code{train_na}}{logical; indicates the joint missing values in the training
#'   IND and pressure data. That includes the original NAs as well as randomly selected
#'   test observations that are within the training period}
#' }
"ind_init_ex"


#' Model output tibble from the \code{\link{model_gam}} function
#'
#' This is an example output tibble from the \code{model_gam} function applied
#' on the Central Baltic Sea food web indicator demonstration data.
#'
#' @format A data frame with 84 rows and 17 variables:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{model_type}}{Specification of the modeltype; at this stage containing only
#'              "gam" (Generalized Additive Model).}
#'   \item{\code{corrstruc}}{Specification of the correlation structure; at this stage
#'              containing only "none".}
#'   \item{\code{aic}}{AIC of the fitted models}
#'   \item{\code{edf}}{Estimated degrees of freedom for the model terms.}
#'   \item{\code{p_val}}{The p values for the smoothing term (the pressure).}
#'   \item{\code{signif_code}}{The significance codes for the p-values.}
#'   \item{\code{r_sq}}{The adjusted r-squared for the models. Defined as the proportion
#'               of variance explained, where original variance and residual variance are
#'               both estimated using unbiased estimators. This quantity can be negative
#'               if your model is worse than a one parameter constant model, and can be
#'               higher for the smaller of two nested models.}
#'   \item{\code{expl_dev}}{The proportion of the null deviance explained by the models.}
#'   \item{\code{nrmse}}{Absolute values of the Normalized Root Mean Square Error (NRMSE).}
#'   \item{\code{ks_test}}{The p-values from a Kolmogorov-Smirnov Test applied on the model
#'               residuals to test for normal distribution. P-values > 0.05 indicate
#'               normally distributed residuals.}
#'   \item{\code{tac}}{logical; indicates whether temporal autocorrelation (TAC) was detected
#'               in the residuals. TRUE if model residuals show TAC.}
#'   \item{\code{pres_outlier}}{A list-column with outliers identified for each model (i.e.
#'               cook's distance > 1). The indices present the position in
#'               the training data, including NAs.}
#'   \item{\code{excl_outlier}}{A list-column listing all outliers per model that have been
#'               excluded in the GAM fitting}
#'   \item{\code{model}}{A list-column of IND~press-specific gam objects.}
#' }
"model_gam_ex"


#' Model output tibble from the \code{\link{model_gamm}} function
#'
#' This is an example output tibble from the \code{model_gamm} function applied
#' on the Central Baltic Sea food web indicator demonstration data.
#'
#' @format A data frame with 234 rows and 16 variables:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{model_type}}{Specification of the modeltype; at this stage containing only
#'              "gam" (Generalized Additive Model).}
#'   \item{\code{corrstruc}}{Specification of the correlation structure; at this stage
#'              containing only "none".}
#'   \item{\code{aic}}{AIC of the fitted models}
#'   \item{\code{edf}}{Estimated degrees of freedom for the model terms.}
#'   \item{\code{p_val}}{The p values for the smoothing term (the pressure).}
#'   \item{\code{signif_code}}{The significance codes for the p-values.}
#'   \item{\code{r_sq}}{The adjusted r-squared for the models. Defined as the proportion
#'               of variance explained, where original variance and residual variance are
#'               both estimated using unbiased estimators. This quantity can be negative
#'               if your model is worse than a one parameter constant model, and can be
#'               higher for the smaller of two nested models.}
#'   \item{\code{nrmse}}{Absolute values of the Normalized Root Mean Square Error (NRMSE).}
#'   \item{\code{ks_test}}{The p-values from a Kolmogorov-Smirnov Test applied on the model
#'               residuals to test for normal distribution. P-values > 0.05 indicate
#'               normally distributed residuals.}
#'   \item{\code{tac}}{logical; indicates whether temporal autocorrelation (TAC) was detected
#'               in the residuals. TRUE if model residuals show TAC.}
#'   \item{\code{pres_outlier}}{A list-column with outliers identified for each model (i.e.
#'               cook's distance > 1). The indices present the position in
#'               the training data, including NAs.}
#'   \item{\code{excl_outlier}}{A list-column listing all outliers per model that have been
#'               excluded in the GAM fitting}
#'   \item{\code{model}}{A list-column of IND~press-specific gam objects.}
#' }
"model_gamm_ex"


#' Output tibble from the \code{\link{merge_models}} function
#'
#' This is an example output tibble from the \code{merge_models} function applied
#' on the Central Baltic Sea food web indicator demonstration data. More
#' specifically, the function is applied on a subset of the \code{model_gam_ex}
#' tibble (including only GAMs with no temporal autocorrelation) and the
#' \code{model_gamm_ex} after selecting the best GAMMs using
#' \code{\link{select_model}}.
#'
#' @format A data frame with 84 rows and 17 variables:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{model_type}}{Specification of the modeltype; at this stage containing only
#'              "gam" (Generalized Additive Model).}
#'   \item{\code{corrstruc}}{Specification of the correlation structure; at this stage
#'              containing only "none".}
#'   \item{\code{aic}}{AIC of the fitted models}
#'   \item{\code{edf}}{Estimated degrees of freedom for the model terms.}
#'   \item{\code{p_val}}{The p values for the smoothing term (the pressure).}
#'   \item{\code{signif_code}}{The significance codes for the p-values.}
#'   \item{\code{r_sq}}{The adjusted r-squared for the models. Defined as the proportion
#'               of variance explained, where original variance and residual variance are
#'               both estimated using unbiased estimators. This quantity can be negative
#'               if your model is worse than a one parameter constant model, and can be
#'               higher for the smaller of two nested models.}
#'   \item{\code{expl_dev}}{The proportion of the null deviance explained by the models.}
#'   \item{\code{nrmse}}{Absolute values of the Normalized Root Mean Square Error (NRMSE).}
#'   \item{\code{ks_test}}{The p-values from a Kolmogorov-Smirnov Test applied on the model
#'               residuals to test for normal distribution. P-values > 0.05 indicate
#'               normally distributed residuals.}
#'   \item{\code{tac}}{logical; indicates whether temporal autocorrelation (TAC) was detected
#'               in the residuals. TRUE if model residuals show TAC.}
#'   \item{\code{pres_outlier}}{A list-column with outliers identified for each model (i.e.
#'               cook's distance > 1). The indices present the position in
#'               the training data, including NAs.}
#'   \item{\code{excl_outlier}}{A list-column listing all outliers per model that have been
#'               excluded in the GAM fitting}
#'   \item{\code{model}}{A list-column of IND~press-specific gam objects.}
#' }
"merge_models_ex"


#' Output tibble after applying all IND~pressure modelling functions
#'
#' This is an example output tibble based on the Central Baltic Sea
#' food web indicator demonstration data after applying the \code{\link{calc_deriv}}
#' and the \code{\link{test_interaction}} functions on the
#' \code{merge_models_ex} tibble.
#'
#' @format A data frame with 84 rows and 31 variables:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{model_type}}{Specification of the modeltype; at this stage containing only
#'              "gam" (Generalized Additive Model).}
#'   \item{\code{corrstruc}}{Specification of the correlation structure; at this stage
#'              containing only "none".}
#'   \item{\code{aic}}{AIC of the fitted models}
#'   \item{\code{edf}}{Estimated degrees of freedom for the model terms.}
#'   \item{\code{p_val}}{The p values for the smoothing term (the pressure).}
#'   \item{\code{signif_code}}{The significance codes for the p-values.}
#'   \item{\code{r_sq}}{The adjusted r-squared for the models. Defined as the proportion
#'               of variance explained, where original variance and residual variance are
#'               both estimated using unbiased estimators. This quantity can be negative
#'               if your model is worse than a one parameter constant model, and can be
#'               higher for the smaller of two nested models.}
#'   \item{\code{expl_dev}}{The proportion of the null deviance explained by the models.}
#'   \item{\code{nrmse}}{Absolute values of the Normalized Root Mean Square Error (NRMSE).}
#'   \item{\code{ks_test}}{The p-values from a Kolmogorov-Smirnov Test applied on the model
#'               residuals to test for normal distribution. P-values > 0.05 indicate
#'               normally distributed residuals.}
#'   \item{\code{tac}}{logical; indicates whether temporal autocorrelation (TAC) was detected
#'               in the residuals. TRUE if model residuals show TAC.}
#'   \item{\code{pres_outlier}}{A list-column with outliers identified for each model (i.e.
#'               cook's distance > 1). The indices present the position in
#'               the training data, including NAs.}
#'   \item{\code{excl_outlier}}{A list-column listing all outliers per model that have been
#'               excluded in the GAM fitting}
#'   \item{\code{model}}{A list-column of IND~press-specific gam objects.}
#'   \item{\code{prop}}{The proportion of the observed pressure range where the IND
#'              indicator shows a response (see the last section in \emph{Details})}
#'   \item{\code{zero_in_conf}}{A list-column of logical vectors indicating for
#'              every pressure value (in press_seq) whether the slope of the IND
#'              response at that pressure value is within the confidence interval,
#'              i.e. is zero.}
#'   \item{\code{zic_start_end}}{A list-column of logical vectors indicating for
#'              every pressure value (in press_seq) whether the slope is considered
#'              as zero for the proportion calculation (see see the last section in
#'              \emph{Details})}.
#'   \item{\code{press_seq}}{A list-column with sequences of evenly spaced pressure
#'              values (with the length of the time series).}
#'   \item{\code{pred}}{A list-column with the predicted indicator responses
#'              averaged across all bootstraps.}
#'   \item{\code{pred_ci_up}}{A list-column with the upper confidence limit of the
#'              bootstrapped predictions.}
#'   \item{\code{pred_ci_low}}{A list-column with the lower confidence limit of the
#'              bootstrapped predictions.}
#'   \item{\code{deriv1}}{A list-column with the first derivatives of the indicator responses
#'              averaged across all bootstraps.}
#'   \item{\code{deriv1_ci_up}}{A list-column with the upper confidence limit of the
#'              bootstrapped first derivatives.}
#'   \item{\code{deriv1_ci_low}}{A list-column with the lower confidence limit of the
#'              bootstrapped first derivatives.}
#'   \item{\code{adj_n_boot}}{The number of successful bootstrap samples that was
#'              actually used for calculating the mean and confidence intervals of
#'              the predicted indicator reponse and the derivative.}
#'   \item{\code{boot_error}}{A list-column capturing potential error messages that
#'              occurred as side effects when refitting the GAM(M)s on each bootstrap
#'              sample.}
#'   \item{\code{interaction}}{logical; if TRUE, at least one thresh_gam
#'              performs better than its corresponding gam based on the leave-one-out
#'              cross-validation.}
#'   \item{\code{thresh_var}}{A list-column with the threshold variables of the
#'              better performing thresh_models.}
#'   \item{\code{thresh_models}}{A list-column with nested lists containing the
#'              better performing thresh_models.}
#'   \item{\code{thresh_error}}{A list-column capturing potential error messages that
#'              occurred as side effects when fitting each threshold GAMs and performing the
#'              LOOCV.}
#'   \item{\code{tac_in_thresh}}{logical vector; indicates for every listed
#'              thresh_model whether temporal autocorrelation (TAC) was
#'              detected in the residuals. TRUE if model residuals show TAC.}
#' }
"all_results_ex"

