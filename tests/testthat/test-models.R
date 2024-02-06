test_that("wtf_fit_models works", {
  # These are very basic tests; we'd probably like to do something better
  x <- wtf_fit_models(cars$speed, cars$dist)
  expect_s3_class(x, "data.frame")

  # Produces warnings, but returns a data frame, for perfect-fit data
  suppressWarnings(
    expect_warning(y <- wtf_fit_models(1:3, 1:3))
  )
  expect_s3_class(y, "data.frame")
  expect_true(is.na(y$r.squared_poly))
})
