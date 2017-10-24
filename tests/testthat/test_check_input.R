context("test check_input")

y <- data.frame(a = rnorm(10), b = rpois(10, 3))
y_mat <- as.matrix(y)
y_tib <- tibble::as_tibble(y)

test_that("correct coercion", {
  expect_equal(class(y), class(check_input(y)))
  expect_equal(class(y), class(check_input(y_mat)))
  expect_equal(class(y), class(check_input(y_tib)))
})

y <- data.frame(a = c(1, 2, 3), b = as.factor(c("a",
  "b", "c")))

test_that("error message", {
  expect_error(check_input(y))
})
