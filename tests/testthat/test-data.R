rf <- system.file("extdata", "sst.tif", package = "vapour", mustWork = TRUE)

test_that("raw data works", {
  expect_equal(2 * 2, 4)
})
