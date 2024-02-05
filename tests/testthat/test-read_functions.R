test_that("wtf_read_LI7810 works", {

  # Good data
  x <- wtf_read_LI7810("data/TG10-01087-good.data", quiet = TRUE)
  expect_s3_class(x, "data.frame")
  expect_true(is.na(x$CO2[1])) # parsed missing values
  expect_true("SN" %in% names(x)) # parsed serial number from header

  suppressMessages({
    expect_message(wtf_read_LI7810("data/TG10-01087-good.data", quiet = FALSE))
  })

  # Bad data
  expect_error(wtf_read_LI7810("data/TG10-01087-bad.data", quiet = TRUE),
               regexp = "does not appear")
})

test_that("wtf_read_LI7820 works", {

  # Good data
  x <- wtf_read_LI7820("data/TG20-01182-good.data", quiet = TRUE)
  expect_s3_class(x, "data.frame")
  expect_true("SN" %in% names(x)) # parsed serial number from header

})
