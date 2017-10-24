context("test ind_init")

ind_ex$Year <- as.factor(ind_ex$Year)
test <- ind_init(press = press_ex[, -1], ind = ind_ex[,
  -1], time = ind_ex[, 1])
test2 <- ind_ex[1:15, -1]

test_that("check errors and warnings", {
  expect_error(ind_init(press = press_ex[, -1], ind = ind_ex[,
    -1], time = ind_ex[, 1], train = 1.1))
  expect_warning(ind_init(press = press_ex[, -1],
    ind = test2, time = ind_ex[, 1]))
  expect_equal(length(test$ind_train[[1]]) + length(test$ind_test[[1]]),
    length(ind_ex[, 1]))
  expect_is(test$ind_train[[2]], class(ind_ex[, 2]))
  expect_equivalent(vapply(test[3, 1:3], class, character(1)),
    c("integer", "character", "character"))
  expect_equal(nrow(ind_init_ex), 84)
})

test3 <- ind_init(press = press_ex[, -1], ind = ind_ex[ ,-1],
  time = ind_ex[, 1], train = 0.5, random = TRUE)

test_that("test the NA vector", {
  expect_equal(length(test$train_na[[1]]), 27)
  expect_equal(sum(test$train_na[[1]]), 0)
  expect_equal(sum(test3$train_na[[1]] == FALSE),
    15)
})

