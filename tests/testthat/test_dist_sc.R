context("test dist_sc")

scores_ex <- scoring(trend_tbl = model_trend_ex,
	 mod_tbl = all_results_ex,
  press_type = INDperform::press_type_ex)
dat <- dist_sc(scores_ex, method_dist = "euclidean")

test_that("test structure of returned object", {
  expect_s3_class(dat, "dist")
  expect_true(is.numeric(dat))
})

test_that("error messages", {
  expect_error(dist_sc(scores_ex, method_dist = "complete"))
	 # method in clust_sc
})

