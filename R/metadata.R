# metadata.R


#' Convenience function to fill in missing measurement metadata
#'
#' @param metadat Metadata, data frame
#' @param dead_band_default Default dead band value, numeric (seconds)
#' @param obs_length_default Default observation length value, numeric (seconds)
#' @return The metadata data frame, with missing values filled in.
#' @export
#' @examples
#' metadat <- data.frame(Plot = 1:2, Dead_band = c(1, NA))
#' metadat <- wtf_fill_metadata(metadat)
#' # The missing entry is the 'Dead_band' column has been changed to the default
#' # (60), and a new column 'Obs_length' has been created and filled in
wtf_fill_metadata <- function(metadat,
                              dead_band_default = 10,
                              obs_length_default = 60) {

  stopifnot(is.data.frame(metadat))
  stopifnot(is.numeric(dead_band_default))
  stopifnot(is.numeric(obs_length_default))

  # Fill in defaults, if necessary
  if("Dead_band" %in% names(metadat)) {
    isna <- is.na(metadat$Dead_band)
    if(any(isna)) {
      wtf_message("Replacing ", sum(isna), " empty Dead_band values with default ", dead_band_default)
      metadat$Dead_band[isna] <- dead_band_default
    }
  } else {
    wtf_message("Creating Dead_band column")
    metadat$Dead_band <- dead_band_default
  }

  if("Obs_length" %in% names(metadat)) {
    isna <- is.na(metadat$Obs_length)
    if(any(isna)) {
      wtf_message("Replacing ", sum(isna), " empty Obs_length values with default ", obs_length_default)
      metadat$Obs_length[isna] <- obs_length_default
    }
  } else {
    wtf_message("Creating Obs_length column")
    metadat$Obs_length <- obs_length_default
  }

  return(metadat)
}


#' Match metadata info with a vector of data timestamps
#'
#' @param data_timestamps Data timestamps, either character (YYYY-MM-DD HH:MM:SS) or \code{\link{POSIXct}}
#' @param start_dates Metadata measurement date entries, either character (YYYY-MM-DD) or \code{\link{POSIXct}}
#' @param start_times Metadata measurement start time entries, either character (HH:MM:SS) or \code{\link[lubridate]{period}}
#' @param dead_bands Dead band lengths in seconds, numeric; must be same length as \code{start_dates}
#' @param obs_lengths Observation lengths in seconds, numeric; must be same length as \code{start_dates}
#' @importFrom lubridate ymd_hms ymd hms tz is.POSIXct is.period
#' @importFrom utils head tail
#' @importFrom stats na.omit
#' @return A numeric vector equal in length to \code{data_timestamps}, with
#' each entry indicating the metadata entry that should be used for that
#' observation. \code{NA} is returned if a timestamp has no match in the metadata (i.e., does not
#' fall within any window defined by the \code{start_dates}, \code{start_times},
#' and observation length parameters).
#' @export
#' @examples
#' # Data timestamps
#' d_t <- c("2024-01-01 13:00:05", "2024-01-01 13:00:10",
#' "2024-01-01 13:05:05", "2024-01-01 13:10:00")
#' # Metadata start dates and times: two measurements, starting 5 minutes apart
#' s_d <- c("2024-01-01", "2024-01-01")
#' s_t <- c("13:00:00", "13:05:00")
#' db <- c(1, 1) # Dead bands
#' ol <- c(60, 60) # Observation lengths
#' wtf_metadata_match(d_t, s_d, s_t, db, ol)
#' # Returns {1, 1, 2, NA} indicating that the first and second data timestamps
#' # correspond to metadata entry 1, the third to entry 2, and the fourth
#' # has no match
#'
#' # This generates an error because of overlapping timestamps:
#' \dontrun{
#' s_t <- c("13:00:00", "13:01:00")
#' wtf_metadata_match(d_t, s_d, s_t, db, ol)
#' }
wtf_metadata_match <- function(data_timestamps,
                               start_dates, start_times,
                               dead_bands, obs_lengths) {

  # Input checks and convert to dates/timestamps if needed
  stopifnot(length(start_dates) == length(start_times))
  stopifnot(length(start_dates) == length(dead_bands))
  stopifnot(length(start_dates) == length(obs_lengths))

  # The metadata dates and times shouldn't be empty
  if(any(is.na(start_dates))) {
    wtf_warning("One or more metadata dates are missing")
  }
  if(any(is.na(start_times))) {
    wtf_warning("One or more metadata times are missing")
  }

  # Convert things to POSIXct and check validity
  if(is.character(data_timestamps)) data_timestamps <- ymd_hms(data_timestamps)
  stopifnot(is.POSIXct(data_timestamps))

  if(is.character(start_dates)) start_dates <- ymd(start_dates,
                                                   tz = tz(data_timestamps))
  stopifnot(is.POSIXct(start_dates))

  if(is.character(start_times)) start_times <- hms(start_times)
  stopifnot(is.period(start_times))

  stopifnot(is.numeric(dead_bands))
  stopifnot(is.numeric(obs_lengths))

  # Compute the metadata start and stop timestamps
  start_timestamps <- start_dates + start_times + dead_bands
  stopifnot(is.POSIXct((start_timestamps))) # should always be true!
  stop_timestamps <- start_timestamps + obs_lengths

  # Metadata records shouldn't overlap; we assume that metadata is from a
  # single machine that generated the data
  ord <- order(start_timestamps)
  overlaps <- head(stop_timestamps[ord], -1) >= tail(start_timestamps[ord], -1)
  if(any(overlaps, na.rm = TRUE)) {
    stop("start_timestamps overlaps: ",
         paste(which(overlaps) + 1, collapse = ", "))
  }

  # Compute data-metadata matches
  matches <- rep(NA_real_, length(data_timestamps))
  entries <- seq_along(start_timestamps)
  for(i in entries) {
    mtch <- data_timestamps >= start_timestamps[i] & data_timestamps <= stop_timestamps[i]
    matches[mtch] <- i
  }

  # Metadata rows with zero matches seems unexpected
  no_matches <- base::setdiff(entries, unique(matches, na.omit(matches)))
  if(length(no_matches)) {
    wtf_message("Entries with no timestamp matches: ", paste(no_matches, collapse = ", "))
  }

  return(matches)
}
