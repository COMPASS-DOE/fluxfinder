# qaqc.R


#' Generate a QA/QC document
#'
#' @param flux_data A data frame from \code{\link{ffi_compute_fluxes}} or similar
#' @param group_column Name of the grouping label column in \code{flux_data},
#' character; pass NULL to run with no grouping
#' @param output_file Name of the output file
#' @param output_dir Name of the output directory; default is current working directory
#' @param open_output Automatically open the output HTML file?
#' @importFrom utils browseURL
#' @return The path of the output file
#' @export
#'
#' @examples
#' # Read smart chamber concentration data
#' f <- system.file("extdata/LI8200-01S.json", package = "fluxfinder")
#' dat <- ffi_read_LIsmartchamber(f)
#' dat <- dat[dat$RepNum == 1,]
#'
#' # Unit conversion
#' dat$CO2_umol <- ffi_ppm_to_umol(dat$co2,
#'   volume = dat$TotalVolume[1] / 100 ^ 3,
#'   temp = dat$chamber_t[1])
#' dat$CO2_umol_m2 <- dat$CO2_umol /  0.0314 # normalize by area (20 cm collar)
#'
#' # Compute fluxes
#' fluxes <- ffi_compute_fluxes(dat, group_column = "label",
#'   time_column = "TIMESTAMP", gas_column = "CO2_umol_m2", dead_band = 5)
#'
#' # Now generate the QAQC page
#' x <- ffi_qaqc(fluxes, group_column = "label")
#' file.remove(x) # clean up
ffi_qaqc <- function(flux_data,
                     group_column,
                     output_file = "qaqc.html",
                     output_dir = getwd(),
                     open_output = TRUE) {
  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("To run this function, please install the rmarkdown package")
  }

  # Save the flux data into a temporary file so as to pass the
  # fully-qualified filename as a parameter to our Rmarkdown file
  f <- system.file("qaqc.Rmd", package = "fluxfinder")
  td <- tempdir()
  tf_flux_data <- file.path(td, "flux_data")
  saveRDS(flux_data, tf_flux_data)

  # Render
  fout <- rmarkdown::render(f,
                            output_file = output_file,
                            output_dir = output_dir,
                            quiet = ffi_isquiet(),
                            params = list(flux_data = tf_flux_data,
                                          group_column = group_column))
  if(open_output) browseURL(paste0('file://', fout))
  invisible(fout)
}
