context("test cond_boot")


# Test helper functions ---------------------------
new_n_boot <- check_n_boot(n_boot = 50, ci = 1.5)

test_that("test check_n_boot", {
  expect_false(new_n_boot == 50)
  expect_message(check_n_boot(n_boot = 50, ci = 1.5))
  expect_failure((expect_message(check_n_boot(n_boot = 40,
    ci = 1.5))))
})

test_list <- list(ind_init_ex)

set.seed(1)
a <- tibble::tibble(
	id=1:3,
	test_list = list(x1 = rnorm(20), x2 = rnorm(20), x3 = rnorm(20))
	) %>% list()

test <- apply(cbind(a[[1]]$test_list[[1]], a[[1]]$test_list[[2]],
  a[[1]]$test_list[[3]]), 1, mean)

test_that("test calc_value", {
  expect_true(is.numeric(calc_value(a, "test_list", mean)))
  expect_true(length(calc_value(a, "test_list", mean)) ==
    length(a[[1]]$test_list[[1]]))
  expect_equal(calc_value(a, "test_list", mean), test)
})


# -------------------------------------------------


# Test actual cond_boot() function
id <- ind_init_ex$id[(ind_init_ex$ind == "TZA" & ind_init_ex$press ==
  "Fsprat") | (ind_init_ex$ind == "Stickle" & ind_init_ex$press ==
  "Ssum")]
a <- ind_init_ex[id, ]
b <- merge_models_ex[id, ]

dat <- cond_boot(init_tbl = a, mod_tbl = b, n_boot = 40,
	 excl_outlier = FALSE, ci = 0.95,
  par_comp = FALSE, no_clust = NULL, seed = NULL)

test_that("output tibble", {
  expect_s3_class(dat, "tbl_df")
  # test columns
  expect_equal(ncol(dat), 24)
  expect_true(all(names(b) %in% names(dat)))
  expect_true(all(c("pred_ci_up", "pred_ci_low",
    "deriv1_ci_low", "deriv1_ci_up") %in% names(dat)))
  # test rows
  expect_true(all(b$id %in% dat$id))
  # test generated data
  expect_is(dat$press_seq, "list")
  expect_is(dat$pred, "list")
  expect_is(dat$deriv1, "list")
  expect_is(dat$press_seq[[1]], "numeric")
  expect_is(dat$deriv1[[1]], "numeric")
  expect_is(dat$deriv1_ci_low[[1]], "numeric")
  expect_is(dat$deriv1_ci_up[[1]], "numeric")
})

# Test whether the seed setting works for both
# serial and parallel computing
id <- ind_init_ex$id[(ind_init_ex$ind == "TZA" & ind_init_ex$press ==
  "Fsprat")]
a <- ind_init_ex[id, ]
b <- merge_models_ex[id, ]

ser1 <- cond_boot(a, b, n_boot = 40, ci = 0.95, excl_outlier = FALSE,
  par_comp = FALSE, no_clust = NULL, seed = 1)
ser2 <- cond_boot(a, b, n_boot = 40, ci = 0.95, excl_outlier = FALSE,
  par_comp = FALSE, no_clust = NULL, seed = 1)
# Works fine but not when checking the package with devtools:
# par1 <- cond_boot(a, b, n_boot = 40, ci = 0.95, excl_outlier = FALSE,
#   par_comp = TRUE, no_clust = NULL, seed = 1)
# par2 <- cond_boot(a, b, n_boot = 40, ci = 0.95, excl_outlier = FALSE,
#   par_comp = TRUE, no_clust = NULL, seed = 1)

test_that("seed setting", {
  expect_equal(ser1$pred, ser2$pred)
  expect_equal(ser1$deriv1, ser2$deriv1)
  expect_equal(ser1$deriv1_ci_up, ser2$deriv1_ci_up)

  # expect_equal(par1$pred, par2$pred)
  # expect_equal(par1$deriv1, par2$deriv1)
  # expect_equal(par1$deriv1_ci_up, par2$deriv1_ci_up)

  #expect_true(ser1$pred[[1]][1] != par1$pred[[1]][1])
})

