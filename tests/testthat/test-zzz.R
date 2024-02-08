test_that("quiet functionality works", {
  # Check that the wtf_message and wtf_warning functions respect quiet option
  withr::local_options(whattheflux.quiet = TRUE)
  expect_no_message(wtf_message("hi"))
  expect_no_warning(wtf_warning("hi"))

  withr::local_options(whattheflux.quiet = FALSE)
  expect_message(wtf_message("hi"), regexp = "hi")
  expect_warning(wtf_warning("hi"), regexp = "hi")
})
