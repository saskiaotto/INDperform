context("test calc_deriv")

a <- ind_init_ex[c(1, 9, 48), ]
b <- merge_models_ex[c(1, 9, 48), ]
b2 <- b
b2$model[[2]] <- NA
b3 <- b
b3$edf <- as.character(b3$edf)

test_that("test warnings and errors", {
  expect_error(calc_deriv(a, b2))
  expect_error(calc_deriv(a[-1, ], b))
  expect_error(calc_deriv(a, b, ci_boot = 1.5))
  expect_error(calc_deriv(a, b, ci_boot = NULL))
  expect_error(calc_deriv(a, b, edf_filter = NULL))
  expect_error(calc_deriv(a, b, edf_filter = 1))
  expect_error(calc_deriv(a, b, p_val_filter = 1.5))
  expect_error(calc_deriv(a, b, p_val_filter = NULL))
  expect_error(calc_deriv(a, b, edf_filter = 1))
  expect_error(calc_deriv(a, b, method = "conditional_boot"))
  expect_error(calc_deriv(a, b, par_comp = TRUE,
    no_cores = -1))
  expect_error(calc_deriv(a, b, method = "cond_boot",
    par_comp = 1))
  expect_error(calc_deriv(a, b, method = "cond_boot",
    par_comp = "yes"))
  expect_error(calc_deriv(a, b, seed = "sometext"))
  expect_error(calc_deriv(a, b[ ,-16], excl_outlier = TRUE))
  expect_message(calc_deriv(a, b, edf_filter = 10))
  # check of input tibbles
  expect_error(calc_deriv(a[, -(1:3)], b)) # init_tbl missing variables
  expect_error(calc_deriv(a, b[, -(1:3)])) # mod_tbl missing variables
  expect_error(calc_deriv(a, b3)) # wrong data type in required variable

  # that should not return an error (all required variables in mod_tbl), just
  # a message that no edf > edf_filter
  expect_message(calc_deriv(ind_init_ex[1, ],
  	b[1, c("id", "ind", "press", "corrstruc","edf", "p_val", "excl_outlier", "model")]))
})

a <- ind_init_ex[c(1, 9), ]
b <- merge_models_ex[c(1, 9), ]
dat <- calc_deriv(a, b, n_boot = 40)

test_that("output tibble", {
  expect_s3_class(dat, "tbl_df")
  # test columns
  expect_equal(ncol(dat), 27)
  expect_true(all(names(b) %in% names(dat)))
  # test rows
  expect_true(all(b$id %in% dat$id))
  # test generated data
  expect_is(dat$prop, "numeric")
  expect_true((dat$prop[[1]] <= 1 & dat$prop[[1]] >=
    0) | is.na(dat$prop[[1]]))
  expect_is(dat$zero_in_conf, "list")
  expect_is(dat$zero_in_conf[[2]], "logical")
})


dat2 <- calc_deriv(a, b, n_boot = 40, method = "cond_boot")
dat3 <- calc_deriv(a, b, n_boot = 40, method = "approx_deriv")

test_that("test method", {
  expect_true(all(c("pred_ci_low", "pred_ci_up") %in%
    names(dat)))
  expect_true(all(c("pred_ci_low", "pred_ci_up") %in%
    names(dat2)))
  expect_true(all(c("pred_ci_low", "pred_ci_up") %in%
    names(dat3)) == FALSE)
})


	 mod_tbl <- check_input_tbl(
				mod_tbl, tbl_name = "mod_tbl", parent_func = "model_gam() or model_gamm()/select_model()",
				var_to_check = c("id", "ind", "press", "corrstruc","edf", "p_val", "excl_outlier", "model"),
				dt_to_check = c("integer", "character", "character", "character", "numeric", "numeric",
					 "list", "list")
		)
