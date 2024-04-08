test_that("quiet functionality works", {
  # Check that the ffi_message and ffi_warning functions respect quiet option
  withr::local_options(fluxfinder.quiet = TRUE)
  expect_no_message(ffi_message("hi"))

  withr::local_options(fluxfinder.quiet = FALSE)
  expect_message(ffi_message("hi"), regexp = "hi")
})
