test_that("ffi_ppm_to_umol works", {
  withr::local_options(fluxfinder.quiet = TRUE)

  # Fixed-value test at STP, assuming our ideal gas law implementation is correct
  # 0.18 is the slope (ppm CO2/s) of the TG10-01087 example data
  x <- ffi_ppm_to_umol(0.18, volume = 0.1)
  expect_equal(round(x, 4), 0.7483)

  # Colder temperature means larger flux for a given concentration rise
  expect_gt(ffi_ppm_to_umol(0.18, volume = 0.1, temp = 5), x)
  # Lower pressure means smaller flux for a given concentration rise
  expect_lt(ffi_ppm_to_umol(0.18, volume = 0.1, atm = 75000), x)
  # Larger volume means larger flux for a given concentration rise
  expect_gt(ffi_ppm_to_umol(0.18, volume = 1), x)

  # Not sure what else to test
})

test_that("ffi_ppb_to_nmol works", {
  withr::local_options(fluxfinder.quiet = TRUE)

  # ffi_ppb_to_nmol is just a pass-through to ffi_ppm_to_umol, so
  # should produce identical results
  expect_identical(ffi_ppm_to_umol(0.18, volume = 0.1),
                   ffi_ppb_to_nmol(0.18, volume = 0.1))
})
