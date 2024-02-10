test_that("wtf_metadata_match works", {

  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")

  # Handles bad input - length and types
  expect_error(wtf_metadata_match(d_t, start_dates = 1, start_times = 1:2),
               regexp = "length\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, 1, 1, obs_lengths = 1:2),
               regexp = "length\\(obs_lengths\\)")
  expect_error(wtf_metadata_match(1:2, 1, 1),
               regexp = "is.POSIXct\\(data_timestamps\\)")
  expect_error(wtf_metadata_match(d_t, 1, "12:34"),
               regexp = "is.POSIXct\\(start_dates\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", 1),
               regexp = "is.period\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", "12:34", obs_lengths = "1"),
               regexp = "is.numeric\\(obs_lengths\\)")

  # Overlapping metadata
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00", "13:05")
  expect_silent(wtf_metadata_match(d_t, s_d, s_t, obs_length_default = 60))
  expect_error(wtf_metadata_match(d_t, s_d, s_t, obs_length_default = 600),
               regexp = "overlaps")
  # Reports which entries are problematic
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-01")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00", "13:01", "13:05"),
                                  obs_length_default = 60),
               regexp = "overlaps: 2")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00", "13:04", "13:05"),
                                  obs_length_default = 60),
               regexp = "overlaps: 3")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00", "13:01", "13:02"),
                                  obs_length_default = 60),
               regexp = "overlaps: 2, 3")

  # Assigns matches correctly
  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00", "13:05")
  expect_silent(x <- wtf_metadata_match(d_t, s_d, s_t))
  expect_identical(x, c(1, 1, 2, NA_integer_))

  # Gives a message if there are unmatched metadata entries
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-10")
  s_t <- c("13:00", "13:05", "13:10")
  expect_message(wtf_metadata_match(d_t, s_d, s_t),
                 regexp = "no timestamp matches: 3")
})
