
test_that("wtf_metadata_match works", {

  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")

  # Handles bad input - length and types
  expect_error(wtf_metadata_match(d_t, start_dates = 1, start_times = 1:2),
               regexp = "length\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, 1, 1, obs_lengths = 1:2),
               regexp = "length\\(obs_lengths\\)")
  expect_error(wtf_metadata_match(1:2, 1, 1, 1),
               regexp = "is.POSIXct\\(data_timestamps\\)")
  expect_error(wtf_metadata_match(d_t, 1, "12:34", 1),
               regexp = "is.POSIXct\\(start_dates\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", 1, 1),
               regexp = "is.period\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", "12:34:00", obs_lengths = "1"),
               regexp = "is.numeric\\(obs_lengths\\)")

  # Warn on missing data
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00:00", "13:05:00")
  suppressMessages({
    expect_warning(wtf_metadata_match(d_t, c("2024-01-01", NA), s_t, 1:2),
                   regexp = "dates are missing")
    expect_warning(wtf_metadata_match(d_t, s_d, c(NA, "13:05:00"), 1:2),
                   regexp = "times are missing")
    expect_warning(wtf_metadata_match(d_t, s_d, s_t, c(NA, 2)),
                   regexp = "observation lengths are missing")
  })

  # Overlapping metadata
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00:00", "13:05:00")
  expect_silent(wtf_metadata_match(d_t, s_d, s_t, obs_lengths = c(60, 60)))
  expect_error(wtf_metadata_match(d_t, s_d, s_t, obs_lengths = c(600, 600)),
               regexp = "overlaps")
  # Reports which entries are problematic
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-01")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:01:00", "13:05:00"),
                                  obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 2")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:04:00", "13:05:00"),
                                  obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 3")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:01:00", "13:02:00"),
                                  obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 2, 3")

  # Assigns matches correctly
  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00:00", "13:05:00")
  ol <- c(60, 60)
  expect_silent(x <- wtf_metadata_match(d_t, s_d, s_t, ol))
  expect_identical(x, c(1, 1, 2, NA_real_))

  # Gives a message if there are unmatched metadata entries
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-10")
  s_t <- c("13:00:00", "13:05:00", "13:10:00")
  expect_message(wtf_metadata_match(d_t, s_d, s_t, c(60, 60, 60)),
                 regexp = "1 entry had no timestamp matches")
})
