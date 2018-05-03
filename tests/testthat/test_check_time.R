context("test check_time")

y1 <- 1:10
y2 <- as.factor(1:10)
y3 <- seq(1,2, 0.1)
y4 <- letters
y5 <- cbind(y1,y1)

test_that("correct return", {
	expect_equal(typeof(y1), typeof(check_time(y1)))
	expect_equal(typeof(y3), typeof(check_time(y3)))
})

test_that("error messages", {
	expect_error(check_time(y2), "not a factor!") # partial matches
	expect_error(check_time(y4), "has to be a NUMERIC vector!")
	expect_error(check_time(y5), "has to be a VECTOR!")
})
