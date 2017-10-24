context("test select_model")

gam_tbl <- model_gam_ex
gamm_tbl <- model_gamm_ex
test <- select_model(gam_tbl, gamm_tbl)
gam_tbl2 <- model_gam_ex[model_gam_ex$id == 1:3, ]
gamm_tbl2 <- model_gamm_ex[model_gamm_ex$id == 43,
  ]


test_that("check warnings if ids are in gamm_tbl but not in gam_tbl", {
  expect_error(select_model(gam_tbl2, gamm_tbl2))
})

test_that("check if merge of gam and gamm works", {
  expect_equal(nrow(test), length(unique(model_gamm_ex$id)))
  expect_equal(names(test), unique(names(gam_tbl),
    names(gamm_tbl)))
  expect_true(all(model_gamm_ex$aic[2:6] + c(2, 4,
    4, 6, 6) > test$aic[1], na.rm = TRUE))
})
