context("test calc_nrmse")

# Get pred values
model_list <- model_gam_ex$model
obs_press <- ind_init_ex$press_test
pred <- calc_pred(model_list, obs_press)$pred
# Observated values
obs_ind <- ind_init_ex$ind_test
dat <- calc_nrmse(pred, obs_ind)

# Calculate one nmrse manual to compare
temp <- (pred[[1]] - obs_ind[[1]])^2
temp <- sum(temp, na.rm = TRUE)
temp <- temp/length(obs_ind[[1]])
temp <- sqrt(temp)
nmrse_gam <- temp/mean(obs_ind[[1]], na.rm = TRUE)

# Get pred values
model_list <- model_gamm_ex$model
dummy <- dplyr::left_join(model_gamm_ex[, 1:3], ind_init_ex,
  by = c("ind", "press"))
obs_press <- dummy$press_test
pred <- calc_pred(model_list, obs_press)$pred
# Observated values
obs_ind <- dummy$ind_test
dat2 <- calc_nrmse(pred, obs_ind)

# Calculate one nmrse manual to compare
temp <- (pred[[1]] - obs_ind[[1]])^2
temp <- sum(temp, na.rm = TRUE)
temp <- temp/length(obs_ind[[1]])
temp <- sqrt(temp)
nmrse_gamm <- temp/mean(obs_ind[[1]], na.rm = TRUE)

test_that("test calc_nrmse", {
  expect_is(dat, "numeric")
  expect_equal(dat[1], nmrse_gam)
  expect_is(dat2, "numeric")
  expect_equal(dat2[1], nmrse_gamm)
})
