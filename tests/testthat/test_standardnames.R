context("running standard names")

test_that(
  desc = "Names are standardized",
  code =
    expect_equal(
      object = tibble::tibble(
                `base élèves` = c("toto"),
                `2017/07/07` = as.Date("2017-07-01")
                ) %>%
            tricky::set_standard_names() %>%
            names(),
      expected = c("base_eleves", "var_2017_07_07")
    )
  )
