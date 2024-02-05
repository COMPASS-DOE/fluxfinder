

#' Internal utility function to read LI-78x0 files
#'
#' @param file Filename to read, character
#' @param model Model name, string
#' @param quiet Be quiet? Logical
#' @importFrom utils read.table
#' @importFrom lubridate ymd_hms
#' @return A data frame with the parsed data.
wtf_read_LI78x0 <- function(file, model, quiet) {

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

  if(!quiet) message("Read ", nrow(dat), " rows of ", sn, " data, ",
                     min(dat$TIMESTAMP), " to ", max(dat$TIMESTAMP), " ", tz)

  return(dat)
}

#' Read a LI-7810 data file
#'
#' @param file Filename to read, character
#' @param quiet Be quiet? Logical
#' @return A data frame with the parsed data.
#' @export
#' @examples
#' f <- system.file("extdata/TG10-01087.data", package = "whattheflux")
#' dat <- wtf_read_LI7810(f)
wtf_read_LI7810 <- function(file, quiet = FALSE) {
  wtf_read_LI78x0(file, "LI-7810", quiet)
}

#' Read a LI-7820 data file
#'
#' @param file Filename to read, character
#' @param quiet Be quiet? Logical
#' @return A data frame with the parsed data.
#' @export
#' @examples
#' f <- system.file("extdata/TG20-01182.data", package = "whattheflux")
#' dat <- wtf_read_LI7820(f)
wtf_read_LI7820 <- function(file, quiet = FALSE) {
  wtf_read_LI78x0(file, "LI-7820", quiet)
}
