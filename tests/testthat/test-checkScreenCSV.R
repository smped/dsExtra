test_that("checkScreenCSV Errors Correctly", {
  f <- system.file("extdata", "test.csv", package = "dsExtra")
  expect_error(checkScreenCSV(basename(f)))
  expect_false(checkScreenCSV(f, "Compound"))
  expect_message(checkScreenCSV(f, "Col"),  "^Possible matches.+")
  expect_message(checkScreenCSV(f, "Col"),  "^The column 'Col' is NOT in this file.")
  expect_message(checkScreenCSV(f, "ol1"),  ".+Is this correct.+")

})

test_that("checkScreenCSV Runs Correctly", {
  f <- system.file("extdata", "test.csv", package = "dsExtra")
  expect_true(checkScreenCSV(f, "Col1"))
})
