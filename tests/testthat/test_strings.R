library(tricky)
context("String functions")

test_that(
  desc = "str_standardize works...",
  code = {
    expect_equal(
      object = str_standardize("code externe de l'action"),
      expected = "code_externe_de_l_action"
    )
    expect_equal(
      object = str_standardize(string = "é,è"),
      expected = "e_e"
    )
    }
  )
