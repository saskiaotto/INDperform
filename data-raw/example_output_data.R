# Generate function output datasets (.rda files)

### Output from model_trend()
model_trend_ex <- model_trend(ind = ind_ex[, -1], time = ind_ex[, 1])

### Initialization of ind~press tibble
ind_init_ex <- ind_init(press = press_ex[, -1], ind = ind_ex[, -1],
	time = ind_ex[, 1])

### Apply simple gam function
model_gam_ex <- model_gam(ind_init_ex)
# Remove outlier
model_gam_ex <- model_gam(ind_init_ex, excl_outlier = model_gam_ex$pres_outlier)

### Apply gamm function with correlation structure for those models with tac
model_gamm_ex <- model_gamm(ind_init_ex, filter = model_gam_ex$tac)
# Remove outlier
model_gamm_ex <- model_gamm(ind_init_ex, excl_outlier = model_gamm_ex$pres_outlier, filter = model_gam_ex$tac)
# Select best corrstructure
select_model_ex <- select_model(model_gam_ex, model_gamm_ex)

### Merge both gam and gamm tibbles
merge_models_ex <-merge_models(model_gam_ex[model_gam_ex$tac==FALSE, ],
																															select_model_ex)

### Calculate the derivatives for non-linear responses and the pressure range
boot_deriv_ex <- calc_deriv(init_tbl = ind_init_ex,
	mod_tbl = merge_models_ex, n_boot = 200, par_comp = TRUE, seed=1)

### Test whether significant pressures interact with other pressures
# (so we have all data output together)
all_results_ex <- test_interaction(init_tbl = ind_init_ex, mod_tbl = boot_deriv_ex,
	interactions = select_interaction(boot_deriv_ex), excl_outlier = TRUE)


# Modify the NRMSE so it is based on the original scale of the untransformed indicator time series:
# log-transformed: TZA, rCC, Cops, Micro, Sprat, Herring, Stickle, Cod, SPF, LPF

all_results_ex$nrmse <- calc_nrmse(
	press =  ind_init_ex$press_test,
	ind = ind_init_ex$ind_test,
	model = all_results_ex$model,
 transformation = c(rep("log",7), rep("none",7), rep("log",3*7), rep("none",7), rep("log",6*7))
)


devtools::use_data(model_trend_ex,
																			ind_init_ex,
																			model_gam_ex,
																			model_gamm_ex,
																			merge_models_ex,
																			all_results_ex,
																			overwrite = TRUE)

rm(list = ls())
