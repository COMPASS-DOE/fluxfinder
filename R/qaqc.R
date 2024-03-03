# qaqc.R


#' Generate a QA/QC document
#'
#' @param flux_data A data frame from \code{\link{wtf_compute_fluxes}} or similar
#' @param group_column Name of the grouping label column in \code{flux_data},
#' character; pass NULL to run with no grouping
#' @param flux_column Name of the column with flux estimates; defaults to
#' "slope_estimate", the output of \code{\link{wtf_compute_fluxes}}
#' @param r2_column Name of the column with adjusted R2 values; defaults to
#' "adj.r.squared", the output of \code{\link{wtf_compute_fluxes}}
#' @param open_output Automatically open the output HTML file?
#' @param ... Other parameters passed on to \code{\link[rmarkdown]{render}}
#' @importFrom utils browseURL
#' @return The path of the output file
#' @export
#'
#' @examples
#' # Toy data
#' cars$Plot <- LETTERS[1:5]
#' fd <- wtf_compute_fluxes(cars, "Plot", "speed", "dist")
#' x <- wtf_qaqc(fd, group_column = "Plot")
#' file.remove(x) # clean up
#' # See the introductory vignette for a fully-worked example with real data
wtf_qaqc <- function(flux_data,
                     group_column,
                     flux_column = "slope_estimate",
                     r2_column = "adj.r.squared",
                     open_output = TRUE,
                     ...) {
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
                            quiet = wtf_isquiet(),
                            params = list(flux_data = tf_flux_data,
                                          group_column = group_column,
                                          flux_column = flux_column,
                                          r2_column = r2_column),
                            ...)
  if(open_output) browseURL(paste0('file://', fout))
  invisible(fout)
}
