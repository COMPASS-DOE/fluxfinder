
#' Read a LI-7810 data file
#'
#' @param file Filename to read
#' @param quiet Be quiet? Logical
#' @importFrom utils read.table
#' @importFrom lubridate ymd_hms
#' @return A data frame with the parsed data.
#' @export
wtf_read_LI7810 <- function(file, quiet = FALSE) {
  basefn <- basename(file)
  if(!quiet) message("Processing ", basefn)

  dat_raw <- readLines(file)

  # Make sure this is a 7810 file
  model <- trimws(gsub("Model:\t", "", dat_raw[1], fixed = TRUE))
  if(model != "LI-7810") stop("This does not appear to be a LI-7810 file!")

  # Save the machine serial number
  sn <- trimws(gsub("SN:\t", "", dat_raw[2], fixed = TRUE))
  # Parse the timezone from the header and use it to make a TIMESTAMP field
  tz <- trimws(gsub("Timezone:\t", "", dat_raw[5], fixed = TRUE))

  # These files have five header lines, then the names of the columns in line 6,
  # and then the column units in line 7. We only want the names
  dat_raw <- dat_raw[-c(1:5, 7)]
  # Irritatingly, the units line can repeat in the file. Remove these instances
  dat_raw <- dat_raw[grep("DATAU", dat_raw, invert = TRUE)]
  # Double irritatingly, if there's no remark, the software writes \t\t, not
  # \t""\t, causing a read error. Replace these instances
  dat_raw <- gsub("\t\t", "\tnan\t", dat_raw, fixed = TRUE)

  dat <- read.table(textConnection(dat_raw), na.strings = "nan", header = TRUE)

  # If the try() above succeeded, we have a data frame and can process it
  dat$TIMESTAMP <- lubridate::ymd_hms(paste(dat$DATE, dat$TIME), tz = tz)
  dat$TZ <- tz

  if(!quiet) message("\tRead in ", nrow(dat), " rows of data, ",
                     min(dat$TIMESTAMP), " to ", max(dat$TIMESTAMP))
  if(!quiet) message("\tInstrument serial number: ", sn)
  dat$SN <- sn
  if(!quiet) message("\tInstrument time zone: ", tz)

  # Remove unneeded Licor DATE and TIME columns
  dat$DATE <- dat$TIME <- NULL

  return(dat)
}
