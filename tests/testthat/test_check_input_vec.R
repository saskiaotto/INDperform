context("test check_input_vec")

y1 <- 1:10
y2 <- as.factor(1:10)
y3 <- seq(1,2, 0.1)
y4 <- letters
y5 <- cbind(y1,y1)

test_that("correct return", {
	expect_equal(typeof(y1), typeof(check_input_vec(y1, "y")))
	expect_equal(typeof(y3), typeof(check_input_vec(y3, "y")))
})

test_that("error messages", {
	expect_error(check_input_vec(y2, "y"), "not a factor!") # partial matches
	expect_error(check_input_vec(y4, "y"), "has to be a NUMERIC vector!")
	expect_error(check_input_vec(y5, "y"), "has to be a VECTOR!")
})
