context("test plot_statespace_ed")

# Test data input validation
test_that("test error messages", {
	 expect_error(plot_statespace_ed(x = 1:10), "'x' has to be a tibble")
	expect_error(plot_statespace_ed(x = as.list(1:10)), "'x' has to be a tibble")
})
