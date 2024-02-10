# metadata_match.R

#' Match metadata info with a vector of data timestamps
#'
#' @param data_timestamps Data timestamps, either character (YYYY-MM-DD HH:MM:SS) or \code{\link{POSIXct}}
#' @param start_dates Metadata measurement date entries, either character (YYYY-MM-DD) or \code{\link{POSIXct}}
#' @param start_times Metadata measurement start time entries, either character (HH:MM) or \code{\link[lubridate]{period}}
#' @param obs_length_default Default length of observations, in seconds (numeric)
#' @param obs_lengths Optional vector of custom observation lengths in seconds;
#' if provided must be the same length as \code{start_dates}.
#' Missing values will be filled in with the \code{obs_length_default} value
#' @importFrom lubridate ymd_hms ymd hm tz is.POSIXct is.period
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
#' s_t <- c("13:00", "13:05")
#' wtf_metadata_match(d_t, s_d, s_t)
#' # Returns {1, 1, 2, NA} indicating that the first and second data timestamps
#' # correspond to metadata entry 1, the third to entry 2, and the fourth
#' # has no match
#'
#' # This generates an error because of overlapping timestamps:
#' \dontrun{
#' s_t <- c("13:00", "13:01")
#' wtf_metadata_match(d_t, s_d, s_t, obs_length_default = 120)
#' }
wtf_metadata_match <- function(data_timestamps,
                               start_dates, start_times,
                               obs_length_default = 60,
                               obs_lengths = rep(obs_length_default,
                                                 length(start_dates))) {

  # Input checks and convert to dates/timestamps if needed
  stopifnot(length(start_dates) == length(start_times))
  stopifnot(length(start_dates) == length(obs_lengths))

  if(is.character(data_timestamps)) data_timestamps <- ymd_hms(data_timestamps)
  stopifnot(is.POSIXct(data_timestamps))

  if(is.character(start_dates)) start_dates <- ymd(start_dates,
                                                   tz = tz(data_timestamps))
  stopifnot(is.POSIXct(start_dates))

  if(is.character(start_times)) start_times <- hm(start_times)
  stopifnot(is.period(start_times))

  stopifnot(is.numeric(obs_lengths))

  # Compute the metadata start and stop timestamps
  start_timestamps <- start_dates + start_times
  stopifnot(is.POSIXct((start_timestamps))) # should always be true!
  stop_timestamps <- start_timestamps + obs_lengths

  # Metadata records shouldn't overlap; we assume that metadata is from a
  # single machine that generated the data
  ord <- order(start_timestamps)
  overlaps <- head(stop_timestamps[ord], -1) >= tail(start_timestamps[ord], -1)
  if(any(overlaps)) {
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
