context("running formatting")

test_that("unfrench formatting returns the good outcome", {
  expect_equal(object = unfrench_formatting(x = "0,12"),
               expected = 0.12)

  expect_equal(object = unfrench_formatting(x = "1 200,24"),
               expected = 1200.24)

  })

test_that("french formatting returns the good outcome", {
  expect_equal(object = french_formatting(x = 0.12),
              expected = "0,12")
  expect_equal(object = french_formatting(x = 1012.24),
               expected = "1 012,24")
  })


test_that("percent formatting returns the good outcome", {
  expect_equal(object = percent_formatting(0.12),
             expected = "12 %")
  })
