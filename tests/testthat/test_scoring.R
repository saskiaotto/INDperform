context("test scoring")

dat <- scoring(trend_tbl = model_trend_ex,
	 mod_tbl = all_results_ex,
  press_type = INDperform::press_type_ex)
dat <- tidyr::unnest(dat)
ind_v <- unique(dat$ind)
crit_v <- names(dat) %in% c("C8", "C11")
subcrit_v <- c("C9_1", "C9_2", "C10_1", "C10_2", "C10_3",
  "C10_4")
press_type_ex2 <- press_type_ex
names(press_type_ex2)[2] <- "something_else"

# Change conditions to create error messages
crit_scores_mod1 <- crit_scores_tmpl
crit_scores_mod1[4, "condition"] <- "x < 0.3"
crit_scores_mod2 <- crit_scores_tmpl
crit_scores_mod2[23, "condition"] <- "x == 2"


test_that("test structure of returned object", {
  expect_is(dat, "tbl_df")
  # all ind and press included?
  expect_identical(sort(unique(dat$ind)),
  	 sort(unique(model_trend_ex$ind)))
  expect_identical(sort(unique(dat$ind)),
  	 sort(unique(all_results_ex$ind)))
  expect_identical(sort(unique(dat$press)),
  	 sort(unique(all_results_ex$press)))
  expect_identical(sort(unique(dat$press)),
  	 sort(unique(all_results_ex$press)))
  # all (sub)crit included?
  expect_equal(sum(names(dat) %in% c("C8", "C11")), 2)
  expect_equal(sum(subcrit_v %in% names(dat)), 6)
})

test_that("test scores", {
  expect_lte(max(dat[, c(2, 3, 7:12)]), 3)
  expect_true(sum(is.na(dat[, c(2, 3, 7:12)])) ==
    0)
})


# DIV: Test for error and warning messages
all_results_ex2 <- all_results_ex
all_results_ex2$edf <- as.character(all_results_ex2$edf)
all_results_ex3 <- all_results_ex
all_results_ex3$edf <- NA_real_

test_that("test error messages and warnings", {

	 # Missing arguments
	expect_error(scoring(press_type = press_type_ex, trend_tbl = model_trend_ex),
  	 "Argument 'mod_tbl' is missing")

	  # Check of input tibbles
  expect_error(scoring(mod_tbl = all_results_ex, press_type = press_type_ex),
  	 "You must provide data for the 'trend_tbl'") # trend_tbl missing but needed (C8)
 	expect_error(scoring(trend_tbl = model_trend_ex, press_type = press_type_ex)) # mod_tbl missing (always needed)
	 expect_error(scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex)) # press_type missing (needed for C11)
		expect_error(scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex,
			 press_type = press_type_ex[,-1]), "press_type has to be a data frame or tibble")
  expect_error(scoring(trend_tbl = model_trend_ex, mod_tbl = all_results_ex,
			 press_type = press_type_ex2), "The following variables required ")

	# Check if all pressures in mod_tbl are also in press_type
	 expect_error(scoring(trend_tbl = model_trend_ex,
    mod_tbl = all_results_ex, press_type = INDperform::press_type_ex[-(1:3), ]),
	  	"are not listed in the 'press_type' tibble")

	 # Check if variables present in crit scores are missing in trend_tbl/mod_tbl
		expect_error(scoring(trend_tbl = dplyr::select(model_trend_ex, -p_val),
    mod_tbl = all_results_ex, press_type = INDperform::press_type_ex),
	  	"are not provided in 'trend_tbl'")
							expect_error(scoring(trend_tbl = dplyr::select(model_trend_ex, -ind),
    mod_tbl = all_results_ex, press_type = INDperform::press_type_ex),
	  	"missing in 'trend_tbl'")
							expect_error(scoring(trend_tbl = model_trend_ex,
    mod_tbl = dplyr::select(all_results_ex, -edf), press_type = INDperform::press_type_ex),
	  	"not provided in any of the input tibbles")

			# Check if all ind are present in both input tibbles (if trend_tbl needed)
			expect_error(scoring(trend_tbl = model_trend_ex[-1,], # ind TZA removed in trend_tbl
				mod_tbl = all_results_ex, press_type = INDperform::press_type_ex),
				"Some indicators are only present")

			# Check for correct datatype in mod_tbl
   expect_error(scoring(model_trend_ex, all_results_ex2, INDperform::press_type_ex))

			# Check whether any variable to score is null or NA/NaN
   expect_error(scoring(model_trend_ex, all_results_ex3, INDperform::press_type_ex),
   	 "contains no information")

	# Overlap in conditions (subcrit)
  expect_error(scoring(trend_tbl = model_trend_ex,
    mod_tbl = all_results_ex, press_type = INDperform::press_type_ex,
    crit_scores = crit_scores_mod1))
  # Overlap in conditions (crit 11)
  expect_error(scoring(trend_tbl = model_trend_ex,
    mod_tbl = all_results_ex, press_type = INDperform::press_type_ex,
    crit_scores = crit_scores_mod2))
})


######################### Remove criteria #############


# Remove criteria
crit_scores_mod3 <- crit_scores_tmpl[crit_scores_tmpl$crit %in%
  c("C8", "C9", "C10") & crit_scores_tmpl$subcrit !=
  "C10_3", ]
dat2 <- scoring(trend_tbl = model_trend_ex,
	 mod_tbl = all_results_ex,
  press_type = INDperform::press_type_ex,
	 crit_scores = crit_scores_mod3)
dat2 <- tidyr::unnest(dat2)
ind_v <- unique(dat2$ind)
subcrit_v <- c("C9_1", "C9_2", "C10_1",
	 "C10_2", "C10_4")

dat <- scoring(trend_tbl = model_trend_ex,
	 mod_tbl = all_results_ex,
  press_type = INDperform::press_type_ex)
dat <- tidyr::unnest(dat)
ind_v <- unique(dat$ind)
crit_v <- names(dat) %in% c("C8", "C11")
subcrit_v <- c("C9_1", "C9_2", "C10_1",
	 "C10_2", "C10_3",
  "C10_4")

test_that("test structure of returned object", {
  expect_is(dat2, "tbl_df")
  # all ind and press included?
  expect_identical(sort(unique(dat2$ind)),
  	 sort(unique(model_trend_ex$ind)))
  expect_identical(sort(unique(dat2$ind)),
  	 sort(unique(all_results_ex$ind)))
  expect_identical(sort(unique(dat2$press)),
  	 sort(unique(all_results_ex$press)))
  expect_identical(sort(unique(dat2$press)),
  	 sort(unique(all_results_ex$press)))
  # all (sub)crit included?
  expect_equal(sum(names(dat2) %in% c("C8", "C11")), 1)
  expect_equal(sum(subcrit_v %in% names(dat2)), 5)
})


test_that("test scores", {
  expect_lte(max(dat2[, c(2, 6:10)]), 3)
  expect_true(sum(is.na(dat2[, c(2, 6:10)])) == 0)
})


