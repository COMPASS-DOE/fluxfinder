test_that("ffi_read_LI7810 works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data
  x <- ffi_read_LI7810("data/TG10-01087-good.data")
  expect_s3_class(x, "data.frame")
  expect_true(is.na(x$CO2[1])) # parsed missing values
  expect_true("SN" %in% names(x)) # parsed serial number from header

  # Bad data
  expect_error(ffi_read_LI7810("data/TG10-01087-bad.data"),
               regexp = "does not appear")
})

test_that("ffi_read_LI7820 works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data
  x <- ffi_read_LI7820("data/TG20-01182-good.data")
  expect_s3_class(x, "data.frame")
  expect_true("SN" %in% names(x)) # parsed serial number from header

})

test_that("ffi_read_LGR works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data (1) - month-day-year
  expect_warning(ffi_read_LGR915("data/LGR-good-data-mdy.csv"),
                 regexp = "date_format not provided")
  x <- ffi_read_LGR915("data/LGR-good-data-mdy.csv", date_format = "MDY")
  expect_s3_class(x, "data.frame")
  expect_true(lubridate::month(x[1,1]) == 5)
  expect_true("MODEL" %in% names(x))
  expect_true("SN" %in% names(x)) # parsed serial number from header

  # Good data (2) - day-month-year
  x <- ffi_read_LGR915("data/LGR-good-data-dmy.txt", date_format = "DMY")
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 5L)
  expect_true(lubridate::month(x[1,1]) == 4)
  expect_true("MODEL" %in% names(x))
  expect_true("SN" %in% names(x)) # parsed serial number from header

  # Good data (2) - with PGP block
  x <- ffi_read_LGR915("data/LGR-good-data-pgp.txt", date_format = "DMY")
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 4L)

  # Respects time zone setting
  x <- ffi_read_LGR915("data/LGR-good-data-mdy.csv",
                       date_format = "MDY", tz = "EST")
  expect_true(all(tz(x$Time) == "EST"))
})

test_that("ffi_read_PicarroG2301 works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data
  x <- ffi_read_PicarroG2301("data/PicarroG2301-good-data.dat")
  expect_s3_class(x, "data.frame")
  expect_true("MODEL" %in% names(x))
  # Respects time zone setting
  x <- ffi_read_PicarroG2301("data/PicarroG2301-good-data.dat", tz = "EST")
  expect_true(all(tz(x$TIMESTAMP) == "EST"))
})

test_that("ffi_read_EGM4 works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data
  x <- ffi_read_EGM4("data/EGM4-good-data.dat", 2023)
  expect_s3_class(x, "data.frame")
  expect_true("MODEL" %in% names(x))
  expect_true("SOFTWARE" %in% names(x))
  expect_true(all(lubridate::year(x$TIMESTAMP) == 2023))

  # Respects time zone setting
  x <- ffi_read_EGM4("data/EGM4-good-data.dat", 2023, tz = "EST")
  expect_identical(tz(x$TIMESTAMP[1]), "EST")

  # Bad data
  expect_error(ffi_read_EGM4("data/EGM4-bad-data.dat"),
               regexp = "does not look like")
})

test_that("ffi_read_LIsmartchamber works", {

  withr::local_options(list(fluxfinder.quiet = TRUE))

  # Good data
  x <- ffi_read_LIsmartchamber("data/LI8200-01S-good-data.json")
  expect_s3_class(x, "data.frame")
  expect_true("TIMESTAMP" %in% names(x))
  expect_s3_class(x$TIMESTAMP, "POSIXct")

  # Read fluxes only, no concentrations
  x <- ffi_read_LIsmartchamber("data/LI8200-01S-good-data.json",
                               concentrations = FALSE)
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 4L) # test data has 2 obs x 2 reps
  expect_false("TIMESTAMP" %in% names(x))
})
