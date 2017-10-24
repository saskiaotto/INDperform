context("test loocv")

ind <- ind_init_ex$ind_train[[1]]
press <- ind_init_ex$press_train[[1]]
t_var <- ind_init_ex$press_train[[2]]
time <- ind_init_ex$time_train[[1]]
model <- model_gam_ex$model[[1]]
test <- loocv_thresh_gam(model, ind, press, t_var,
  "Ssum", k = 4, a = 0.2, b = 0.8, time)

test_that("test output loocv for threshold-GAM", {
  expect_false(test)
})
