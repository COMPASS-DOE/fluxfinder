# read_data_functions.R

#' Internal utility function to read LI-78x0 files
#'
#' @param file Filename to read, character
#' @param model Instrument model name, string
#' @details The is an internal function used by \code{\link{ffi_read_LI7810}}
#' and \code{\link{ffi_read_LI7820}}, and not normally called by users.
#' @importFrom utils read.table
#' @importFrom lubridate ymd_hms
#' @return A \code{\link{data.frame}} with the parsed data.
ffi_read_LI78x0 <- function(file, model) {

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
  dat <- read.table(textConnection(dat_raw),
                    na.strings = "nan",
                    header = TRUE,
                    stringsAsFactors = FALSE)
  dat$TIMESTAMP <- lubridate::ymd_hms(paste(dat$DATE, dat$TIME), tz = tz)
  dat$TZ <- tz
  dat$SN <- sn
  dat$MODEL <- model
  dat$DATE <- dat$TIME <- NULL

  ffi_message(basename(file), ": read ", nrow(dat), " rows of ", sn, " data, ",
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
#' f <- system.file("extdata/TG10-01087.data", package = "fluxfinder")
#' dat <- ffi_read_LI7810(f)
ffi_read_LI7810 <- function(file) {
  ffi_read_LI78x0(file, "LI-7810")
}

#' Read a LI-7820 data file
#'
#' @param file Filename to read, character
#' @return A \code{\link{data.frame}} with the parsed data.
#' @details Currently LI-7810 and LI-7820 files are handled identically.
#' @export
#' @examples
#' f <- system.file("extdata/TG20-01182.data", package = "fluxfinder")
#' dat <- ffi_read_LI7820(f)
ffi_read_LI7820 <- function(file) {
  ffi_read_LI78x0(file, "LI-7820")
}


#' Read a LGR 915-0011 data file
#'
#' @param file Filename to read, character
#' @param date_format Date format, character: "MDY" (month-day-year)
#' "DMY" (day-month-year), or "YMD" (year-month-day)
#' @param tz Time zone of the file's time data, character (optional)
#' @return A \code{\link{data.frame}} with the parsed data.
#' @importFrom lubridate dmy_hms mdy_hms ymd_hms
#' @importFrom utils read.csv
#' @details The LGR 915-0011 was an Ultra-Portable Greenhouse Gas Analyzer made
#' by Los Gatos Research. The date in its output files can appear in different
#' formats, which is why the \code{date_format} parameter is needed.
#' @note Some LGR 915 files can have a PGP block at the end; this is ignored.
#' @export
#' @examples
#' f <- system.file("extdata/LGR-data.csv", package = "fluxfinder")
#' dat <- ffi_read_LGR915(f, date_format = "MDY")
#' dat <- ffi_read_LGR915(f, date_format = "MDY", tz = "EST") # specify time zone
ffi_read_LGR915 <- function(file, date_format = "DMY", tz = "UTC") {

  if(missing(date_format)) {
    warning("date_format not provided; assuming ", date_format)
  }

  dat_raw <- readLines(file)

  # Some of the samples files that we have seen contain a PGP encoded block
  # at their end, which is bizarre. Check for this and ignore
  pgp <- grep("BEGIN PGP MESSAGE", dat_raw, fixed = TRUE)
  if(length(pgp) > 0) {
    lastline <- pgp[1] - 2
  } else { # no PGP block
    lastline <- length(dat_raw)
  }

  # A single header line encodes version number, date, and serial number
  dat <- read.csv(textConnection(dat_raw[-1]),
                  check.names = FALSE,
                  stringsAsFactors = FALSE,
                  blank.lines.skip = TRUE,
                  nrows = lastline - 2) # 2 header rows

  # The first column can be named either "Time" or "SysTime", and can be
  # MM/DD/YYYY or DD/MM/YYYY. What a TERRIBLE decision decision, LGR :(
  if(date_format == "DMY") {
    dat[1] <- dmy_hms(dat[,1], tz = tz)
  } else if(date_format == "MDY") {
    dat[1] <- mdy_hms(dat[,1], tz = tz)
  } else if(date_format == "YMD") {
    dat[1] <- ymd_hms(dat[,1], tz = tz)
  } else stop("Unknown date_format")

  sn <- regexpr("SN:[A-Z0-9-]*", dat_raw[1])
  dat$SN <- substr(dat_raw[1], sn + nchar("SN:"), sn + attr(sn, "match.length"))
  dat$MODEL <- "LGR915"
  return(dat)
}

#' Read a Picarro G2301 data file
#'
#' @param file Filename to read, character
#' @param tz Time zone of the file's time data, character (optional)
#' @return A \code{\link{data.frame}} with the parsed data.
#' @importFrom lubridate ymd_hms
#' @importFrom utils read.table
#' @references
#' \url{https://www.picarro.com/environmental/products/g2301_gas_concentration_analyzer}
#' @export
#' @examples
#' f <- system.file("extdata/PicarroG2301-data.dat", package = "fluxfinder")
#' dat <- ffi_read_PicarroG2301(f)
#' dat <- ffi_read_PicarroG2301(f, tz = "EST") # specify time zone
ffi_read_PicarroG2301 <- function(file, tz = "UTC") {

  dat <- read.table(file, header = TRUE, stringsAsFactors = FALSE)

  dat$TIMESTAMP <- ymd_hms(paste(dat$DATE, dat$TIME), tz = tz)
  dat$MODEL <- "G2301"
  return(dat)
}

#' Read an EGM-R data file
#'
#' @param file Filename to read, character
#' @param year Four-digit year of the data (EGM-4 output files have
#' month, day, hour, and minute, but not year), numeric or character
#' @param tz Time zone of the file's time data, character (optional)
#' @return A \code{\link{data.frame}} with the parsed data.
#' @importFrom lubridate ymd_hm
#' @importFrom utils read.table
#' @export
#' @examples
#' f <- system.file("extdata/EGM4-data.dat", package = "fluxfinder")
#' dat <- ffi_read_EGM4(f, 2023)
#' dat <- ffi_read_EGM4(f, 2023, tz = "EST") # specify time zone
ffi_read_EGM4 <- function(file, year, tz = "UTC") {

  dat_raw <- readLines(file)

  if(!grepl("EGM-4", dat_raw[1])) {
    stop("This does not look like an EGM-4 file!")
  }

  dat_raw[3] <- gsub("^;", "", dat_raw[3])
  # We want the header line, so remove its initial semicolon...
  software <- gsub("^;SoftwareVersion=", "", dat_raw[2])
  # ...and then remove all lines that begin with a semicolon
  dat_raw <- dat_raw[grep("^;", dat_raw, invert = TRUE)]

  dat <- read.table(textConnection(dat_raw),
                    header = TRUE,
                    sep = "\t",
                    check.names = FALSE,
                    stringsAsFactors = FALSE)

  dat$SOFTWARE <- software
  dat$MODEL <- "EGM-4"
  dat$TIMESTAMP <- ymd_hm(paste(
    paste(year, dat$Month, dat$Day, sep = "-"),
    paste(dat$Hour, dat$Min, sep = ":")
    ), tz = tz)

  return(dat)
}

#' Read a LI-8200-01S (smart chamber) data file
#'
#' @param file Filename to read, character
#' @param concentrations Return concentration data (the default), or
#' just summary information? Logical
#' @return A \code{\link{data.frame}} with the parsed data.
#' @importFrom jsonlite read_json
#' @importFrom lubridate ymd_hms
#' @note These files are in \href{https://www.json.org/json-en.html}{JSON} format.
#' See also \url{https://www.licor.com/env/products/soil-flux/smart-chamber}.
#' @export
#' @examples
#' f <- system.file("extdata/LI8200-01S.json", package = "fluxfinder")
#' dat <- ffi_read_LIsmartchamber(f) # returns 240 rows
#' ffi_read_LIsmartchamber(f, concentrations = FALSE) # only 4 rows
ffi_read_LIsmartchamber <- function(file, concentrations = TRUE) {

  dat_raw <- jsonlite::read_json(file)

  final_dat <- list()

  # Loop through all observations (e.g., collars)
  for(obs in seq_along(dat_raw$datasets)) {
    ffi_message("Reading observation ", obs)
    dat <- dat_raw$datasets[[obs]][[1]]

    # Loop through all repetitions within an observation
    for(rep in seq_along(dat$reps)) {
      # Info on observation and rep
      rep_df <- data.frame(obs = obs, rep = rep)

      repdat <- dat$reps[[rep]]

      # The header section contains information on instrument,
      # dead band, volume, etc. Store as a 1-row data frame
      header_df <- as.data.frame(repdat$header)

      if(concentrations) {
        # Convert main observational data into a data frame
        data_info <- list()
        for(i in names(repdat$data)) {
          data_info[[i]] <- unlist(repdat$dat[[i]])
        }
        data_df <- as.data.frame(data_info)
      } else {
        data_df <- data.frame(timestamp = 1) # this will get deleted below
      }

      # Convert footer flux info into a 1-row data frame
      footer_info <- list()
      for(i in seq_along(repdat$footer$fluxes)) {
        fdf <- repdat$footer$fluxes[[i]]
        gasname <- fdf$name
        fdf$name <- NULL
        names(fdf) <- paste(gasname, names(fdf), sep = "_")
        footer_info[[i]] <- as.data.frame(fdf)
      }
      footer_df <- as.data.frame(footer_info)

      # Combine and store; note that the 1-row data frames get
      # replicated to have as many rows as the data
      final_dat[[paste(obs, rep)]] <- cbind(rep_df, header_df, data_df, footer_df)
    }
  }

  # Combine everything into a single data frame
  out <- do.call("rbind", final_dat)
  if(concentrations) {
    out$TIMESTAMP <- ymd_hms(out$Date, tz = out$TimeZone[1]) + out$timestamp
  }
  out$timestamp <- NULL # to avoid confusion
  out
}
