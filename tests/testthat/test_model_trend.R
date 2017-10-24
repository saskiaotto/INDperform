context("test model_trend")

dat <- model_trend(ind_ex[, -1], time = ind_ex[, 1])
ind <- ind_ex[, 12]  # SPF
time <- ind_ex[, 1]
model <- mgcv::gam(ind ~ 1 + s(time, k = 4), na.action = "na.omit")
p_val <- get_sum_output(list(mgcv::summary.gam(model)),
  "s.table", cell = 4)

test_that("extract the correct p-values", {
  expect_equal(names(INDperform::ind_ex[, -1]), dat$ind)
  expect_equal(dat$ind_id, 1:ncol(INDperform::ind_ex[,
    -1]))
  expect_equal(model$model$press, dat$model[[11]]$model$press)
  expect_true(length(dat$model[[1]]$model$time) !=
    length(dat$model[[11]]$model$time))
  expect_equal(p_val, dat$p_val[11])
})

dat2 <- model_trend(ind_ex[, -1], time = ind_ex[, 1],
  train = 0.5, random = TRUE)

test_that("test the NA vector", {
  expect_equal(length(dat$model[[1]]$train_na), length(time))
  expect_equal(sum(dat$model[[1]]$train_na), 0)
  expect_equal(sum(dat2$model[[1]]$train_na == FALSE),
    15)
})
