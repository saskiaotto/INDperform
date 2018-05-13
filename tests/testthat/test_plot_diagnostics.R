context("test plot_diagnostics")

model_list <- c(all_results_ex$thresh_models[[13]],
	model_gam_ex$model[39], all_results_ex$model[76])
plots <- plot_diagnostics(model_list)
plots2 <- plot_diagnostics(model_gam_ex$model[39])
model_dfr <- all_results_ex


test_that("test error messages", {
	expect_error(plot_diagnostics(), "Argument 'model_list' is missing" )
	expect_error(plot_diagnostics(model_list = model_dfr) )
})

test_that("structure of returned object", {
	expect_equal(ncol(plots), 9)
	expect_equal(nrow(plots), 3)

	expect_is(plots$ind, "character")
	expect_is(plots$press, "character")
	expect_is(plots$cooks_dist, "list")
	expect_is(plots$acf_plot, "list")
	expect_is(plots$pacf_plot, "list")
	expect_is(plots$resid_plot, "list")
	expect_is(plots$gcvv_plot, "list")
	expect_is(plots$qq_plot, "list")
	expect_is(plots$all_plots, "list")

	expect_is(plots$cooks_dist[[1]], "ggplot")
 # if no threshold-GAM included
	expect_true(is.na(plots2$gcvv_plot[[1]]))
})
