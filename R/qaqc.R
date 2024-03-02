# qaqc.R


#' Generate a QA/QC document
#'
#' @param flux_data A data frame from \code{\link{wtf_compute_fluxes}} or similar
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
#' # Toy data
#' cars$Plot <- c("A", "B")
#' fd <- wtf_compute_fluxes(cars, "Plot", "speed", "dist")
#' x <- wtf_qaqc(fd, group_column = "Plot")
#' file.remove(x) # clean up
#' # See the introductory vignette for a fully-worked example with real data
wtf_qaqc <- function(flux_data,
                     group_column,
                     output_file = "qaqc.html",
                     output_dir = getwd(),
                     open_output = TRUE) {
  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("To run this function, please install the rmarkdown package")
  }

  # Save the flux data into a temporary file so as to pass the
  # fully-qualified filename as a parameter to our Rmarkdown file
  f <- system.file("qaqc.Rmd", package = "whattheflux")
  td <- tempdir()
  tf_flux_data <- file.path(td, "flux_data")
  saveRDS(flux_data, tf_flux_data)

  # Render
  fout <- rmarkdown::render(f,
                            output_file = output_file,
                            output_dir = output_dir,
                            quiet = wtf_isquiet(),
                            params = list(flux_data = tf_flux_data,
                                          group_column = group_column))
  if(open_output) browseURL(paste0('file://', fout))
  invisible(fout)
}
