# models.R

#' Fit various models to gas concentration data
#'
#' @param time Relative time of observation (typically seconds), numeric
#' @param conc Greenhouse gas concentration (typically ppm or ppb), numeric
#' @param area Area covered by the measurement chamber (typically cm2), numeric
#' @param volume Volume of the system
#' (chamber + tubing + analyzer, typically cm3), numeric
#' @return A wide-form \code{\link{data.frame}} with fit statistics for linear,
#' robust linear, and polynomial models.
#' The following columns are returned:
#' * Model statistics \code{r.squared}, \code{adj.r.squared},
#' \code{sigma}, \code{statistic}, and \code{p.value} (see the
#' \code{\link{lm}} documentation for more information);
#' * Slope (flux) statistics \code{slope_estimate}, \code{slope_std.error},
#' \code{slope_statistic}, and \code{slope_p.value}, all from \code{\link{lm}};
#' * Intercept statistics \code{int_estimate}, \code{int_std.error},
#' \code{int_statistic}, and \code{int_p.value}, all from \code{\link{lm}};
#' * Additional diagnostics: \code{slope_estimate_robust}, the slope of a
#' robust linear regression model using \code{\link[MASS]{rlm}};
#' \code{r.squared_poly}, the fraction of variance explained by a
#' second-order polynomial model, and \code{slope_HM1981}, the flux
#' computed by \code{\link{wtf_hm1981}} using nonlinear regression based
#' on one-dimensional diffusion theory following
#' Hutchinson and Mosier (1981) and Nakano et al. (2004).
#' @md
#' @details If a linear model cannot be fit, NULL is returned. If the robust
#' linear and/or polynomial models cannot be fit, then \code{NA} is returned
#' for their particular statistics. By default, extensive details are provided
#' only for the linear fit; for robust linear and polynomial, only the slope
#' and R2 are returned, respectively, as these are intended for QA/QC.
#' @note Normally this is not called
#' directly by users, but instead via \code{\link{wtf_compute_fluxes}}.
#' @references
#' Nakano, T., Sawamoto, T., Morishita, T., Inoue, G., and Hatano, R.:
#' A comparison of regression methods for estimating soil–atmosphere diffusion
#' gas fluxes by a closed-chamber technique, Soil Biol. Biochem.,
#' 36, 107–113, 2004. \url{http://dx.doi.org/10.1016/j.soilbio.2003.07.005}
#'
#' Hutchinson, G. L. and Mosier, A. R.: Improved soil cover method for field
#' measurement of nitrous oxide fluxes, Soil Sci. Soc. Am. J., 45, 311-316,
#' 1981. \url{http://dx.doi.org/10.2136/sssaj1981.03615995004500020017x}
#' @importFrom broom glance tidy
#' @importFrom MASS rlm
#' @importFrom stats lm coefficients
#' @export
#' @examples
#' # Toy data
#' wtf_fit_models(cars$speed, cars$dist)
#' # Real data
#' f <- system.file("extdata/TG10-01087.data", package = "whattheflux")
#' dat <- wtf_read_LI7810(f)[1:75,] # isolate first observation
#' dat$SECONDS <- dat$SECONDS - min(dat$SECONDS) # normalize time to start at 0
#' plot(dat$SECONDS, dat$CO2)
#' wtf_fit_models(dat$SECONDS, dat$CO2)
wtf_fit_models <- function(time, conc, area, volume) {
  # Basic linear model
  try(mod <- lm(conc ~ time))
  if(!exists("mod")) {
    warning("Could not fit linear model")
    return(NULL)
  }

  # Linear model overall metrics. 'glance' produces 12 different ones;
  # we keep the first 5 (adjR2, R2, sigma, statistic, p-value)
  model_stats <- glance(mod)[,1:5]

  # Slope and intercept statistics
  tmod <- tidy(mod)
  slope_stats <- tmod[2,-1]
  names(slope_stats) <- paste("slope", names(slope_stats), sep = "_")
  intercept_stats <- tmod[1,-1]
  names(intercept_stats) <- paste("int", names(intercept_stats), sep = "_")

  # Add robust regression slope as a QA/QC check
  slope_stats$slope_estimate_robust <- tryCatch({
    robust <- rlm(conc ~ time)
    coefficients(robust)[2]
  },
  error = function(e) {
    warning("Could not fit robust linear model")
    NA_real_
  })

  # Add polynomial regression R2 as a QA/QC check
  slope_stats$r.squared_poly <- tryCatch({
    poly <- lm(conc ~ poly(time, 3))
    summary(poly)$r.squared
  },
  error = function(e) {
    warning("Could not fit polynomial model")
    NA_real_
  })

  # Add slope computed using Hutchinson and Mosier (1981) nonlinear regression
  slope_stats$slope_HM1981 <- wtf_hm1981(time, conc)
  if(!is.na(slope_stats$slope_HM1981)) {
    wtf_message("NOTE: slope_HM1981 is non-NA, implying nonlinear data")
  }

  # Combine and return
  return(cbind(model_stats, slope_stats, intercept_stats))
}


#' Compute flux using nonlinear Hutchinson and Mosier (1981) model
#'
#' @param time Time values, numeric
#' @param conc Gas concentration values, numeric
#' @param h Effective chamber height
#' @return Flux estimate; see references for more information.
#' @export
#' @importFrom stats approx
#' @references
#' Hutchinson, G. L. and Mosier, A. R.: Improved soil cover method for field
#' measurement of nitrous oxide fluxes, Soil Sci. Soc. Am. J., 45, 311-316,
#' 1981. \url{http://dx.doi.org/10.2136/sssaj1981.03615995004500020017x}
#' @examples
#' # If data are approximately linear, then NA is returned
#' wtf_hm1981(cars$speed, cars$dist)
#' # If data are nonlinear (saturating) then flux based on gas diffusion theory
#' wtf_hm1981(Puromycin$conc, Puromycin$rate)
wtf_hm1981 <- function(time, conc, h = 1) {
  # Compute slope using Hutchinson and Mosier (1981) nonlinear technique
  vals <- approx(time, conc, xout = c(min(time), mean(time), max(time)), ties = mean)$y
  C0 <- vals[1]
  C1 <- vals[2]
  C2 <- vals[3]
  Tmax <- max(time)

  logterm <- (C1 - C0) / (C2 - C1)
  # This approach is only valid when (C1-C0)/(C2-C1) > 1
  if(logterm > 1) {
    (h * (C1 - C0)) ^ 2 / (0.5 * Tmax * (2 * C1 - C2 - C0)) * log(logterm)
  } else {
    NA_real_
  }
}


#' Normalize a vector of times
#'
#' @param time A vector of time values, either \code{\link{POSIXct}} or numeric
#' @param normalize Normalize the values so that first is zero? Logical
#' @return A numeric vector of normalized values (if \code{normalize_time} is
#' TRUE) or the original vector if not.
#' @keywords internal
wtf_normalize_time <- function(time, normalize = TRUE) {
  if(normalize) {
    as.numeric(time) - as.numeric(min(time, na.rm = TRUE))
  } else {
    time
  }
}


#' Compute fluxes for multiple groups (measurements)
#'
#' @param data A \code{\link{data.frame}} (or tibble or data.table)
#' @param group_column Name of the grouping column in \code{data}, character;
#' pass NULL to run with no grouping
#' @param time_column Name of the time column in \code{data}, character
#' @param conc_column Name of the gas concentration column in \code{data}, character
#' @param dead_band Length of dead band, the equilibration period at the
#' beginning of the time series during which data are ignore, in seconds (numeric)
#' @param normalize_time Normalize the values so that first is zero? Logical
#' @param fit_function Optional flux-fit function;
#' default is \code{\link{wtf_fit_models}}
#' @param ... Other parameters passed to \code{fit_function}
#' @return A data.frame with one row per \code{group_column} value. It will
#' always include the mean, minimum, and maximum values of \code{time_column}
#' for that group, but other
#' columns depend on what is returned by the \code{fit_function}.
#' @seealso \code{\link{wtf_fit_models}}
#' @export
#' @examples
#' # No grouping
#' wtf_compute_fluxes(cars, group_column = NULL, "speed", "dist")
#' # With grouping
#' cars$Plot <- c("A", "B")
#' wtf_compute_fluxes(cars, "Plot", "speed", "dist")
#' # See the introductory vignette for a fully-worked example with real data
wtf_compute_fluxes <- function(data,
                               group_column,
                               time_column,
                               conc_column,
                               dead_band = 0,
                               normalize_time = TRUE,
                               fit_function = wtf_fit_models,
                               ...) {

  # Convert to a data.frame so that we can be sure column-selection code
  # will work as intended; tibbles and data.tables have different behavior
  data <- as.data.frame(data)

  # Split by grouping variable
  if(is.null(group_column)) {
    x <- list(data)
  } else {
    x <- split(data, data[group_column])
  }

  # Compute flux for each sample
  # passing volume and area?
  f <- function(x, ...) {
    x$.norm_time <- wtf_normalize_time(x[,time_column], normalize_time)
    x <- x[x$.norm_time >= dead_band,] # exclude dead band data
    out <- fit_function(x$.norm_time, x[,conc_column], ...)
    out[time_column] <- mean(x[,time_column])
    out[paste0(time_column, "_min")] <- min(x[,time_column])
    out[paste0(time_column, "_max")] <- max(x[,time_column])
    return(out)
  }

  # Apply and combine
  y <- lapply(x, f, ...)
  z <- do.call(rbind, y)

  # Clean up row names, column ordering, etc., and return
  if(!is.null(group_column)) z[group_column] <- names(y)
  row.names(z) <- NULL
  onleft <- c(group_column, time_column)
  return(z[c(onleft, setdiff(names(z), onleft))])
}



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
  wtf_ppm_to_umol(ppb, volume, temp, atm)
}
