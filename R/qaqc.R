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
#' @param time1_column time1_column
#' @param time2_column time2_column
#' @param int_column int_column
#' @param obs_data Observational data, optional; a data.frame of concentration
#' data from which the \code{flux_data} were computed
#' @param obs_time_column obs_time_column
#' @param obs_conc_column obs_conc_column
#' @param open_output Automatically open the output HTML file?
#' @param ... Other parameters passed on to \code{\link[rmarkdown]{render}}
#' @importFrom utils browseURL
#' @return The path of the output file
#' @export
#'
#' @examples
#' # Toy data
#' cars$Plot <- LETTERS[1:5]
#' cars$TIMESTAMP <- cars$speed
#' fd <- wtf_compute_fluxes(cars, "Plot", "TIMESTAMP", "dist", dead_band = 5)
#' x <- wtf_qaqc(fd, group_column = "Plot")
#' file.remove(x) # clean up
#' # Pass in observations as well as fluxes for more complete QA/QC
#' x <- wtf_qaqc(fd, group_column = "Plot",
#'   obs_data = cars, obs_conc_column = "dist")
#' file.remove(x) # clean up
#' # See the introductory vignette for a fully-worked example with real data
wtf_qaqc <- function(flux_data,
                     group_column,
                     flux_column = "slope_estimate",
                     r2_column = "adj.r.squared",
                     time1_column = "TIMESTAMP_min",
                     time2_column = "TIMESTAMP_max",
                     int_column = "int_estimate",
                     obs_data = NULL,
                     obs_time_column = "TIMESTAMP",
                     obs_conc_column = NULL,
                     open_output = TRUE,
                     ...) {

  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("To run this function, please install the rmarkdown package")
  }
  if(!is.null(obs_data) & is.null(obs_conc_column)) {
    stop("If obs_data is provided, obs_conc_column must be specified")
  }

  # Save the flux data into a temporary file so as to pass the
  # fully-qualified filename as a parameter to our Rmarkdown file
  f <- system.file("qaqc.Rmd", package = "whattheflux")
  td <- tempdir()
  tf_flux_data <- file.path(td, "flux_data")
  saveRDS(flux_data, tf_flux_data)

  # If observations are provided, do likewise with them
  if(is.null(obs_data)) {
    tf_obs_data <- ""
  } else {
    tf_obs_data <- file.path(td, "obs_data")
    saveRDS(obs_data, tf_obs_data)
  }

  # Render
  f <- system.file("qaqc.Rmd", package = "whattheflux")
  fout <- rmarkdown::render(f,
                            quiet = wtf_isquiet(),
                            params = list(flux_data = tf_flux_data,
                                          group_column = group_column,
                                          flux_column = flux_column,
                                          r2_column = r2_column,
                                          time1_column = time1_column,
                                          time2_column = time2_column,
                                          int_column = int_column,
                                          obs_data = tf_obs_data,
                                          obs_time_column = obs_time_column,
                                          obs_conc_column = obs_conc_column
                            ),
                            ...)
  if(open_output) browseURL(paste0('file://', fout))
  invisible(fout)
}
