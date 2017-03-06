library(tricky)
context("String functions")

test_that("str_length is number of characters", {
  expect_equal(
    str_standardize("code externe de l'action"),
    "code_externe_de_l_action"
    )
})

