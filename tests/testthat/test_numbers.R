library("tricky")
context("Numbers")

test_that(
  desc = "parse_French_numbers()...",
  code = {
    expect_equal(
      object = parse_French_number(x = "1 123,123"),
      expected = 1123.123
    )
  }
)
