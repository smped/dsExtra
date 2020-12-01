test_that("showNamedCompounds errors and runs", {
  df <- data.frame(Compound_ID = c(1:10, "DHT"))
  expect_error(showNamedCompounds(df, "Col1"))
  expect_equal(showNamedCompounds(df, "Compound_ID"), "DHT")
})
