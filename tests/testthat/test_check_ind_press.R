context("test check_input")

y <- data.frame(a = rnorm(10), b = rpois(10, 3))
y_mat <- as.matrix(y)
y_tib <- tibble::as_tibble(y)
y_vec <- rnorm(10)

class(y_mat)
class(check_ind_press(y_vec))

test_that("correct coercion", {
  expect_equal(class(y), class(check_ind_press(y)))
  expect_equal(class(y), class(check_ind_press(y_mat)))
  expect_equal(class(y), class(check_ind_press(y_tib)))
  expect_equal(class(y), class(check_ind_press(y_vec)))
  expect_true(names(check_ind_press(y_vec)) == "ind")
  expect_true(names(check_ind_press(y_vec, input = "press")) ==
    "press")
})

y2 <- data.frame(a = c(1, 2, 3), b = as.factor(c("a",
  "b", "c")))
y_mat2 <- matrix(letters[1:6], ncol = 2)
y_tib2 <- tibble::as_tibble(y2)
y_vec2 <- letters

test_that("error message", {
  expect_error(check_ind_press(y2), "variables have to be NUMERIC!")
  expect_error(check_ind_press(y_mat2), "variables have to be NUMERIC!")
  expect_error(check_ind_press(y_tib2), "variables have to be NUMERIC!")
  expect_error(check_ind_press(y_vec2), "variables have to be NUMERIC!")
})


# test variable name conversion
wrong_nam <- data.frame(Year = 1:20,
  matrix(1:280, ncol = 14))
names(wrong_nam)[-1] <- c("1bcg", "cbf6", "tza(x)", "ms{x}", "rCC[x]", "co d",
  "spra-t", "herr/ing", "tz#", "micro-!", " rZPPP  ", "t!$%&=*+'#;:,pp",
   "1111bcg", "s-p./f")
mod_nam <- c("x1bcg", "cbf6", "tza_x", "ms_x", "rCC_x", "co_d",
  "spra_t", "herr_ing", "tz", "micro", "rZPPP", "t_pp",
   "x1111bcg", "s_p._f")
test_mod_nam <- check_ind_press(wrong_nam)

test_that("name conversion", {
  expect_true(all(names(test_mod_nam)[-1] == mod_nam))
})
