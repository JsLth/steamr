test_that("Orphan braces fail gracefully", {
  vdf <- "{"
  expect_error(parse_vdf(vdf), "Orphaned")
  vdf <- 'test\n{\n"t" "t"\n{\n}'
  expect_error(parse_vdf(vdf), "Orphaned")
})


test_that("Trailing braces fail gracefully", {
  vdf <- "test\n{"
  expect_error(parse_vdf(vdf), "Trailing")
  vdf <- "test\n{\ntest\n{"
  expect_error(parse_vdf(vdf), "Trailing")
})


test_that("Unexpected end fails gracefully", {
  vdf <- "test"
  expect_error(parse_vdf(vdf), "Unexpected end")
  vdf <- "test\n{\n\"t\" \"t\""
  expect_error(parse_vdf(vdf), "Unexpected end")
})


test_that("valid vdf parses correctly", {
  template <- list(
    someresource = list(
      foo = "bar",
      odd = "record",
      someotherresource = list(baz = "tar")
    )
  )
  expect_identical(template, parse_vdf(test_vdf()))
})


test_that("empty strings work", {
  expect_identical(parse_vdf(""), list())
  # comments and macros
  expect_identical(parse_vdf("# test\n// test"), list())
  expect_identical(parse_vdf("test\n{\n}"), list(test = list()))
})


test_that("key-values work with and without quotes", {
  template <- list(test = list(key = "value"))
  vdf <- "test\n{\nkey value\n}"
  expect_identical(template, parse_vdf(vdf))
  vdf <- "test\n{\n\"key value\"\n}"
  expect_identical(template, parse_vdf(vdf))
})


test_that("excess braces fail gracefully", {
  vdf <- "test\n{\nt t\n}\n}"
  expect_error(parse_vdf(vdf), "Excess")
})
