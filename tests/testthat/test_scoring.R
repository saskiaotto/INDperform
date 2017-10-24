context("test scoring")

dat <- scoring(trend_tbl = model_trend_ex,
	 mod_tbl = all_results_ex,
  press_type = INDperform::press_type_ex)
dat <- tidyr::unnest(dat)
ind_v <- unique(dat$ind)
crit_v <- names(dat) %in% c("C8", "C11")
subcrit_v <- c("C9_1", "C9_2", "C10_1", "C10_2", "C10_3",
  "C10_4")

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

test_that("test error messages", {
  # press_type missing
  expect_error(scoring(trend_tbl = model_trend_ex,
    mod_tbl = all_results_ex))
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


