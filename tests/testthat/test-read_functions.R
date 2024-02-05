test_that("wtf_read_LI7810 works", {

  # Good data
  x <- wtf_read_LI7810("data/TG10-01087-good.data", quiet = TRUE)
  expect_s3_class(x, "data.frame")
  expect_true(is.na(x$CO2[1])) # parsed missing values
  expect_true("SN" %in% names(x)) # parsed serial number from header

  # Bad data
  expect_error(wtf_read_LI7810("data/TG10-01087-bad.data", quiet = TRUE), "does not appear")
})
