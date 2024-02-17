
test_that("wtf_fill_metadata works", {

  # Handles bad input
  expect_error(wtf_fill_metadata(1),
               regexp = "is.data.frame\\(metadat\\)")
  expect_error(wtf_fill_metadata(cars, dead_band_default = "1"),
               regexp = "is.numeric\\(dead_band_default\\)")
  expect_error(wtf_fill_metadata(cars, obs_length_default = "1"),
               regexp = "is.numeric\\(obs_length_default\\)")

  # Creates columns
  x <- data.frame(Obs_length = 1)
  expect_message(y <- wtf_fill_metadata(x, dead_band_default = 2),
                 regexp = "Creating Dead_band")
  expect_true("Dead_band" %in% names(y))
  expect_identical(y$Dead_band, 2)
  x <- data.frame(Dead_band = 1)
  expect_message(y <- wtf_fill_metadata(x, obs_length_default = 2),
                 regexp = "Creating Obs_length")
  expect_true("Obs_length" %in% names(y))
  expect_identical(y$Obs_length, 2)

  # Fills in data
  x <- data.frame(Dead_band = c(1, NA), Obs_length = 60)
  expect_message(y <- wtf_fill_metadata(x, dead_band_default = 2),
                 regexp = "Replacing 1 empty Dead_band")
  expect_identical(dim(x), dim(y))
  expect_identical(y$Dead_band, c(1, 2))
  x <- data.frame(Dead_band = 10, Obs_length = c(NA, 60))
  expect_message(y <- wtf_fill_metadata(x, obs_length_default = 2),
                 regexp = "Replacing 1 empty Obs_length")
  expect_identical(dim(x), dim(y))
  expect_identical(y$Obs_length, c(2, 60))
})

test_that("wtf_metadata_match works", {

  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")

  # Handles bad input - length and types
  expect_error(wtf_metadata_match(d_t, start_dates = 1, start_times = 1:2),
               regexp = "length\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, 1, 1, dead_bands = 1:2),
               regexp = "length\\(dead_bands\\)")
  expect_error(wtf_metadata_match(d_t, 1, 1, dead_bands = 1, obs_lengths = 1:2),
               regexp = "length\\(obs_lengths\\)")
  expect_error(wtf_metadata_match(1:2, 1, 1, 1, 1),
               regexp = "is.POSIXct\\(data_timestamps\\)")
  expect_error(wtf_metadata_match(d_t, 1, "12:34", 1, 1),
               regexp = "is.POSIXct\\(start_dates\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", 1, 1, 1),
               regexp = "is.period\\(start_times\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", "12:34:00", dead_bands = "1", 1),
               regexp = "is.numeric\\(dead_bands\\)")
  expect_error(wtf_metadata_match(d_t, "2023-12-23", "12:34:00", 1, obs_lengths = "1"),
               regexp = "is.numeric\\(obs_lengths\\)")

  # Overlapping metadata
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00:00", "13:05:00")
  expect_silent(wtf_metadata_match(d_t, s_d, s_t, c(1, 1), obs_lengths = c(60, 60)))
  expect_error(wtf_metadata_match(d_t, s_d, s_t, c(1, 1), obs_lengths = c(600, 600)),
               regexp = "overlaps")
  # Reports which entries are problematic
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-01")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:01:00", "13:05:00"),
                                  c(1, 1, 1), obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 2")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:04:00", "13:05:00"),
                                  c(1, 1, 1), obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 3")
  expect_error(wtf_metadata_match(d_t, s_d, c("13:00:00", "13:01:00", "13:02:00"),
                                  c(1, 1, 1), obs_lengths = c(60, 60, 60)),
               regexp = "overlaps: 2, 3")

  # Assigns matches correctly
  d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
           "2024-01-01 13:05:05", "2024-01-01 13:10:00")
  s_d <- c("2024-01-01", "2024-01-01")
  s_t <- c("13:00:00", "13:05:00")
  db <- c(1, 1)
  ol <- c(60, 60)
  expect_silent(x <- wtf_metadata_match(d_t, s_d, s_t, db, ol))
  expect_identical(x, c(1, 1, 2, NA_real_))

  # Warns on missing metadata dates or times
  suppressMessages({
    s_d[1] <- NA
    expect_warning(wtf_metadata_match(d_t, s_d, s_t, db, ol),
                   regexp = "dates are missing")
    s_d[1] <- "2024-01-01"
    s_t[2] <- NA
    # hms() behavior with NAs is different than ymd(), so TWO
    # warnings get generated
    expect_warning(
      expect_warning(wtf_metadata_match(d_t, s_d, s_t, db, ol),
                                  regexp = "times are missing"),
      regexp = "failed to parse")

  })

  # Gives a message if there are unmatched metadata entries
  s_d <- c("2024-01-01", "2024-01-01", "2024-01-10")
  s_t <- c("13:00:00", "13:05:00", "13:10:00")
  expect_message(wtf_metadata_match(d_t, s_d, s_t, c(1, 1, 1), c(60, 60, 60)),
                 regexp = "no timestamp matches: 3")
})
