context("test test_interaction")

init_tbl <- ind_init_ex
mod_tbl <- merge_models_ex[c(4:10),]
# test if test_interaction can work without doing
# every combination
interactions <- select_interaction(mod_tbl)[12:13, ]
test <- test_interaction(init_tbl, mod_tbl, interactions,
  excl_outlier = FALSE, a = 0.1, b = 0.9)

test_error <- ind_init_ex[c(9, 11), ]

thresh_gam <- thresh_gam(ind_vec = init_tbl$ind_train[[1]],
  press_vec = init_tbl$press_train[[5]], t_var = init_tbl$press_train[[3]],
  name_t_var = "Pwin", model = test$model[[2]], k = 4,
  a = 0.1, b = 0.9)

mod_tbl2 <- mod_tbl
mod_tbl2[1, -c(1:4)] <- NA


test_that("test output", {
  expect_true(test$interaction[2])
  expect_true(is.na(test$interaction[1]))
  # test if rows without interaction are empty in
  # $thresh_model:
  expect_true(unique(vapply(test$thresh_models[is.na(test$interaction)],
    is.null, logical(1))))
  # test if TRUE/ FALSE occur as often as rows
  # aviable in interactions:
  expect_true(sum(!is.na(test$interaction)) == nrow(interactions))
  # test the classes of created objects:
  expect_is(test$interaction, "logical")
  expect_true(class(test$thresh_models[2][[1]][[1]])[1] ==
    "thresh_gam")
  test$thresh_models[2][[1]][[1]][[52]] <- NULL
    #  train_na vector not in thresh_gam
  expect_equivalent(thresh_gam, test$thresh_models[2][[1]][[1]])
  expect_equivalent(max(test$thresh_models[2][[1]][[1]]$t_val),
    stats::quantile(init_tbl$press_train[[10]],
      prob = 0.9, na.rm = TRUE))
  # took a as .1 instead of default .2!
  expect_equivalent(min(test$thresh_models[2][[1]][[1]]$t_val),
    stats::quantile(init_tbl$press_train[[10]],
      prob = 0.1, na.rm = TRUE))
})
mod_tbl2 <- mod_tbl
mod_tbl2$press <- as.list(mod_tbl2$press)

test_that("test warnings and errors", {
  # gives error if init_tbl has no values for t_var
  expect_error(test_interaction(test_error, mod_tbl,
    interactions))
  expect_error(test_interaction(init_tbl[-c(1:20), ],
    mod_tbl, interactions))
  # gives error because no interactions are given
  expect_error(test_interaction(init_tbl, mod_tbl))
  expect_error(test_interaction(init_tbl, mod_tbl, sign_level = TRUE))
  expect_error(test_interaction(init_tbl, mod_tbl, sign_level = 1.5))
  expect_error(test_interaction(init_tbl, mod_tbl[ ,-16], excl_outlier = TRUE))

  # missing ind~press in mod_tbl that are in interactions
  expect_error(test_interaction(init_tbl, mod_tbl[2, ], interactions))

  # check of input tibbles
  expect_error(test_interaction(init_tbl[, -(1:3)], mod_tbl, interactions)) # init_tbl missing variables
  expect_error(test_interaction(init_tbl, mod_tbl[, -(1:3)])) # mod_tbl missing variables
  expect_error(test_interaction(init_tbl, mod_tbl2)) # wrong data type in required variable

  # that should not return an error (all required variables in mod_tbl), just
  # a message that no edf > edf_filter
  expect_true(tibble::is.tibble(test_interaction(init_tbl,
  	 mod_tbl[2, c("id", "ind", "press", "model_type", "p_val", "excl_outlier", "model")],
  	 interactions[1,])))
})
