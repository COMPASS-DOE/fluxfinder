# read_data_functions.R

#' Internal utility function to read LI-78x0 files
#'
#' @param file Filename to read, character
#' @param model Instrument model name, string
#' @details The is an internal function used by \code{\link{wtf_read_LI7810}}
#' and \code{\link{wtf_read_LI7820}}, and not normally called by users.
#' @importFrom utils read.table
#' @importFrom lubridate ymd_hms
#' @return A \code{\link{data.frame}} with the parsed data.
wtf_read_LI78x0 <- function(file, model) {

  dat_raw <- readLines(file)

  # Make sure this is the desired model
  file_model <- trimws(gsub("Model:\t", "", dat_raw[1], fixed = TRUE))
  if(file_model != model) stop("This does not appear to be a ", model, " file!")

  # Save the machine serial number
  sn <- trimws(gsub("SN:\t", "", dat_raw[2], fixed = TRUE))
  # Parse the timezone from the header and use it to make a TIMESTAMP field
  tz <- trimws(gsub("Timezone:\t", "", dat_raw[5], fixed = TRUE))

  # These files have five header lines, the column names in line 6,
  # and then the column units in line 7. We only want the names and then data
  dat_raw <- dat_raw[-c(1:5, 7)]
  # Irritatingly, the units line can repeat in the file. Remove these instances
  dat_raw <- dat_raw[grep("DATAU", dat_raw, invert = TRUE)]
  # Double irritatingly, if there's no remark, the software writes \t\t, not
  # \t""\t, causing a read error. Replace these instances
  dat_raw <- gsub("\t\t", "\tnan\t", dat_raw, fixed = TRUE)

  # Read the data, construct TIMESTAMP, add metadata,
  # and remove unneeded LI-COR DATE and TIME columns
  dat <- read.table(textConnection(dat_raw), na.strings = "nan", header = TRUE)
  dat$TIMESTAMP <- lubridate::ymd_hms(paste(dat$DATE, dat$TIME), tz = tz)
  dat$TZ <- tz
  dat$SN <- sn
  dat$MODEL <- model
  dat$DATE <- dat$TIME <- NULL

  wtf_message(basename(file), ": read ", nrow(dat), " rows of ", sn, " data, ",
              min(dat$TIMESTAMP), " to ", max(dat$TIMESTAMP), " ", tz)

  return(dat)
}

#' Read a LI-7810 data file
#'
#' @param file Filename to read, character
#' @return A \code{\link{data.frame}} with the parsed data.
#' @details Currently LI-7810 and LI-7820 files are handled identically.
#' @export
#' @examples
#' f <- system.file("extdata/TG10-01087.data", package = "whattheflux")
#' dat <- wtf_read_LI7810(f)
wtf_read_LI7810 <- function(file) {
  wtf_read_LI78x0(file, "LI-7810")
}

#' Read a LI-7820 data file
#'
#' @param file Filename to read, character
#' @return A \code{\link{data.frame}} with the parsed data.
#' @details Currently LI-7810 and LI-7820 files are handled identically.
#' @export
#' @examples
#' f <- system.file("extdata/TG20-01182.data", package = "whattheflux")
#' dat <- wtf_read_LI7820(f)
wtf_read_LI7820 <- function(file) {
  wtf_read_LI78x0(file, "LI-7820")
}


#' Read a LGR-xxxx data file
#'
#' @param file Filename to read, character
#' @param tz Time zone of the file, character (optional)
#' @return A \code{\link{data.frame}} with the parsed data.
#' @importFrom lubridate mdy_hms
#' @export
#' @examples
#' f <- system.file("extdata/LGR-data.csv", package = "whattheflux")
#' dat <- wtf_read_LGR(f)
wtf_read_LGR <- function(file, tz = "UTC") {
  dat_raw <- readLines(file)

  # A single header line encodes version number, date, and serial number
  dat <- read.csv(textConnection(dat_raw[-1]), check.names = FALSE)
  dat$Time <- mdy_hms(dat$Time)
  dat$SN <- trimws(gsub(".*SN:", "", dat_raw[1]))
  return(dat)
}
