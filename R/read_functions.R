
#' Read a LI-7810 data file
#'
#' @param file Filename to read, character
#' @param quiet Be quiet? Logical
#' @importFrom utils read.table
#' @importFrom lubridate ymd_hms
#' @return A data frame with the parsed data.
#' @export
#' @examples
#' f <- system.file("extdata/TG10-01087.data", package = "whattheflux")
#' dat <- wtf_read_LI7810(f)
wtf_read_LI7810 <- function(file, quiet = FALSE) {

  dat_raw <- readLines(file)

  # Make sure this is a 7810 file
  model <- trimws(gsub("Model:\t", "", dat_raw[1], fixed = TRUE))
  if(model != "LI-7810") stop("This does not appear to be a LI-7810 file!")

  # Save the machine serial number
  sn <- trimws(gsub("SN:\t", "", dat_raw[2], fixed = TRUE))
  # Parse the timezone from the header and use it to make a TIMESTAMP field
  tz <- trimws(gsub("Timezone:\t", "", dat_raw[5], fixed = TRUE))

  # These files have five header lines, the column names in line 6,
  # and then the column units in line 7. We only want the names
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
  dat$DATE <- dat$TIME <- NULL

  if(!quiet) message("Read in ", nrow(dat), " rows of ", sn, " data, ",
                     min(dat$TIMESTAMP), " to ", max(dat$TIMESTAMP), " ", tz)

  return(dat)
}
