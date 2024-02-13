test_that("wtf_read_LI7810 works", {

  withr::local_options(list(whattheflux.quiet = TRUE))

  # Good data
  x <- wtf_read_LI7810("data/TG10-01087-good.data")
  expect_s3_class(x, "data.frame")
  expect_true(is.na(x$CO2[1])) # parsed missing values
  expect_true("SN" %in% names(x)) # parsed serial number from header

  # Bad data
  expect_error(wtf_read_LI7810("data/TG10-01087-bad.data"),
               regexp = "does not appear")
})

test_that("wtf_read_LI7820 works", {

  withr::local_options(list(whattheflux.quiet = TRUE))

  # Good data
  x <- wtf_read_LI7820("data/TG20-01182-good.data")
  expect_s3_class(x, "data.frame")
  expect_true("SN" %in% names(x)) # parsed serial number from header

})

test_that("wtf_read_LGR works", {

  withr::local_options(list(whattheflux.quiet = TRUE))

  # Good data
  x <- wtf_read_LGR915("data/LGR-good-data.csv")
  expect_s3_class(x, "data.frame")
  expect_true("SN" %in% names(x)) # parsed serial number from header
  # Respects time zone setting
  x <- wtf_read_LGR915("data/LGR-good-data.csv", tz = "EST")
  expect_identical(tz(x$Time[1]), "EST")
})
