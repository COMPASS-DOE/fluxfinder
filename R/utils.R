# utils.R


#' Convert gas concentration to quantity using the Ideal Gas Law
#'
#' @param volume System volume (chamber + tubing + analyzer, m3), numeric
#' @param temp Optional chamber temperature (degrees C), numeric
#' @param atm Optional atmospheric pressure (Pa), numeric
#' @return The quantity value, in micromoles (for \code{wtf_ppm_to_umol})
#' or nanomoles (for \code{wtf_ppb_to_nmol}).
#' @references Steduto et al.:
#' Automated closed-system canopy-chamber for continuous field-crop monitoring
#' of CO2 and H2O fluxes, Agric. For. Meteorol., 111:171-186, 2002.
#' \url{http://dx.doi.org/10.1016/S0168-1923(02)00023-0}
#' @note If \code{temp} and/or \code{atm} are not provided, the defaults
#' are NIST normal temperature and pressure.
#' @name ideal-gas-law
NULL

#' @rdname ideal-gas-law
#' @param ppm Gas concentration (ppmv), numeric
#' @examples
#' wtf_ppm_to_umol(400, 0.1)
#' @export
wtf_ppm_to_umol <- function(ppm, volume, temp, atm) {

  if(missing(temp)) {
    temp <- 20
    wtf_message("Assuming temp = ", temp, " C")
  }
  if(missing(atm)) {
    atm <- 101325
    wtf_message("Assuming atm = ", atm, " Pa")
  }

  # Gas constant, from https://en.wikipedia.org/wiki/Gas_constant
  R <- 8.31446261815324	 # m3 Pa K−1 mol−1
  wtf_message("Using R = ", R, " m3 Pa K-1 mol-1")
  TEMP_K <- temp + 273.15

  # Use ideal gas law to calculate micromoles: n = pV/RT
  return(ppm * atm * volume / (R * TEMP_K))
}

#' @rdname ideal-gas-law
#' @param ppb Gas concentration (ppbv), numeric
#' @examples
#' wtf_ppb_to_nmol(400, 0.1)
#' @export
wtf_ppb_to_nmol <- function(ppb, volume, temp, atm) {
  # This function is provided only to make things clear for users;
  # there's no difference between it and the wtf_ppm_to_umol() calculations.
  # Yes, here we could divide ppb by 1000 (to umol), call wtf_ppm_to_umol(),
  # and them multiply the result by 1000 (to nmol), but why?
  wtf_ppm_to_umol(ppb, volume, temp, atm)
}
