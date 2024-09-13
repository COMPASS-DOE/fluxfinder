# models.R

#' Fit various models to gas concentration data
#'
#' @param time Relative time of observation (typically seconds), numeric
#' @param conc Greenhouse gas concentration (typically ppm or ppb), numeric
#' @param area Area covered by the measurement chamber (typically cm2), numeric
#' @param volume Volume of the system
#' (chamber + tubing + analyzer, typically cm3), numeric
#' @return A wide-form \code{\link{data.frame}} with fit statistics for linear
#' ("lin", \code{\link{lm}}), robust linear ("rob", \code{\link[MASS]{rlm}}),
#' polynomial ("poly"), and H&M1981 ("HM81", \code{\link{ffi_hm1981}}) models.
#' The latter is based on an exponential model drawn from one-dimensional
#' diffusion theory; see Hutchinson and Mosier (1981) and Nakano et al. (2004).
#'
#' For each model type, the following columns are returned:
#' * Model statistics \code{AIC}, \code{r.squared}, \code{RMSE},
#' and \code{p.value};
#' * Flux (slope) statistics \code{flux.estimate} and \code{flux.std.error};
#' * Intercept statistics \code{int.estimate} and \code{int.std.error};
#' * For the robust linear regression model only,
#' a logical value \code{converged}.
#' @md
#' @details If a linear model cannot be fit, NULL is returned. If the robust
#' linear and/or polynomial models cannot be fit, then \code{NA} is returned
#' for their particular statistics. The HM1981 approach is only valid for
#' saturating (exponential) data and \code{NA} is returned otherwise.
#' @note Normally this is not called directly by users,
#' but instead via \code{\link{ffi_compute_fluxes}}.
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
#' @importFrom stats lm
#' @export
#' @examples
#' # Toy data - linear
#' ffi_fit_models(cars$speed, cars$dist)
#'
#' # Toy data - nonlinear
#' ffi_fit_models(Puromycin$conc, Puromycin$rate)
#'
#' # Real data
#' f <- system.file("extdata/TG10-01087.data", package = "fluxfinder")
#' dat <- ffi_read_LI7810(f)[1:75,] # isolate first observation
#' dat$SECONDS <- dat$SECONDS - min(dat$SECONDS) # normalize time to start at 0
#' plot(dat$SECONDS, dat$CO2)
#' ffi_fit_models(dat$SECONDS, dat$CO2)
ffi_fit_models <- function(time, conc, area, volume) {
  # Basic linear model
  try(mod <- lm(conc ~ time))
  if(!exists("mod")) {
    warning("Could not fit linear model")
    return(NULL)
  }

  # Linear model overall metrics. 'glance' produces 12 different ones;
  # we keep the first 5 (adjR2, R2, sigma, statistic, p-value)
  lin_model_stats <- glance(mod)[c("r.squared", "sigma", "p.value", "AIC")]

  names(lin_model_stats) <- paste0("lin_", names(lin_model_stats))

    # Slope and intercept statistics
  tmod <- tidy(mod)
  lin_slope_stats <- tmod[2, c("estimate", "std.error")]
  names(lin_slope_stats) <- paste0("lin_flux.", names(lin_slope_stats))
  lin_int_stats <- tmod[1, c("estimate", "std.error")]
  names(lin_int_stats) <- paste0("lin_int.", names(lin_int_stats))

  model_stats <- lin_model_stats
  slope_stats <- lin_slope_stats
  int_stats <- lin_int_stats

  # Add robust regression slope as a QA/QC check
  tryCatch({
    robust <- rlm(conc ~ time)

    rob_model_stats <- glance(robust)[c("sigma", "converged", "AIC")]
    tmod <- tidy(robust)
    rob_slope_stats <- tmod[2, c("estimate", "std.error")]

#    rob_int_stats <- tmod[1, c("estimate", "std.error")]
  },
  error = function(e) {
    warning("Could not fit robust linear model")
    rob_model_stats <- data.frame(sigma = NA_real_, converged = FALSE,
                                  AIC = NA_real_, RMSE = NA_real_)
    rob_slope_stats <- data.frame(estimate = NA_real_, std.error = NA_real_)
    # rob_int_stats <- data.frame(estimate = NA_real_, std.error = NA_real_)
  })

  names(rob_model_stats) <- paste0("rob_", names(rob_model_stats))
  names(rob_slope_stats) <- paste0("rob_flux.", names(rob_slope_stats))
  # names(rob_int_stats) <- paste0("rob_int.", names(rob_int_stats))
  model_stats <- cbind(model_stats, rob_model_stats)
  slope_stats <- cbind(slope_stats, rob_slope_stats)
  # int_stats <- cbind(int_stats, rob_int_stats)

  # Add polynomial regression as a QA/QC check
  poly_model_stats <- data.frame(r.squared = NA_real_, AIC = NA_real_, RMSE = NA_real_)
  if(length(time) > 3) {
    try({
      poly <- lm(conc ~ poly(time, 3))
      poly_model_stats <- glance(poly)[c("r.squared", "sigma", "AIC")]
    })
  }

  names(poly_model_stats) <- paste0("poly_", names(poly_model_stats))
  model_stats <- cbind(model_stats, poly_model_stats)

  # Add slope computed using Hutchinson and Mosier (1981) approach
  slope_stats$HM81_flux.estimate <- ffi_hm1981(time, conc)

  # The HM1981 approach is based on an exponential model, so derive fit
  # statistics by log-transforming the data
  if(is.na(slope_stats$HM81_flux.estimate)) {
    hm81_model_stats <- data.frame(r.squared = NA_real_, sigma = NA_real_,
                                   p.value = NA_real_, AIC = NA_real_)
  } else {
    ffi_message("NOTE: HM81_flux.estimate is not NA, implying nonlinear data")
    # The time values are probably normalized, i.e. starting at zero
    # Add a (presumably) tiny offset so we don't get log(0) errors
    mod <- lm(conc ~ log(time + 0.01))
    hm81_model_stats <- glance(mod)[c("r.squared", "sigma", "p.value", "AIC")]
  }

  names(hm81_model_stats) <- paste0("HM81_", names(hm81_model_stats))
  model_stats <- cbind(model_stats, hm81_model_stats)

  # Change "sigma" to "RMSE"; more intuitive for most users
  names(model_stats) <- gsub("sigma", "RMSE", names(model_stats))

  # Combine, sort columns, and return
  out <- cbind(model_stats, slope_stats, int_stats)
  return(out[sort(names(out))])
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
#' ffi_hm1981(cars$speed, cars$dist)
#' # If data are nonlinear (saturating) then flux based on gas diffusion theory
#' ffi_hm1981(Puromycin$conc, Puromycin$rate)
ffi_hm1981 <- function(time, conc, h = 1) {
  # Compute slope using Hutchinson and Mosier (1981) nonlinear technique
  vals <- approx(time, conc, xout = c(min(time), mean(time), max(time)), ties = mean)$y
  C0 <- vals[1]
  C1 <- vals[2]
  C2 <- vals[3]
  Tmax <- max(time)

  # This approach is only valid when (C1-C0)/(C2-C1) > 1, i.e. saturating
  logterm <- (C1 - C0) / (C2 - C1)
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
ffi_normalize_time <- function(time, normalize = TRUE) {
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
#' @param gas_column Name of the gas (concentration or quantity) column in
#' \code{data}, character
#' @param dead_band Length of dead band, the equilibration period at the
#' beginning of the time series during which data are ignore, in seconds (numeric)
#' @param normalize_time Normalize the values so that first is zero? Logical
#' @param fit_function Optional flux-fit function;
#' default is \code{\link{ffi_fit_models}}
#' @param ... Other parameters passed to \code{fit_function}
#' @return A data.frame with one row per \code{group_column} value. It will
#' always include the mean, minimum, and maximum values of \code{time_column}
#' for that group, but other
#' columns depend on what is returned by the \code{fit_function}.
#' @seealso \code{\link{ffi_fit_models}}
#' @export
#' @examples
#' # No grouping
#' ffi_compute_fluxes(cars, group_column = NULL, "speed", "dist")
#' # With grouping
#' cars$Plot <- c("A", "B")
#' ffi_compute_fluxes(cars, "Plot", "speed", "dist")
#' # See the introductory vignette for a fully-worked example with real data
ffi_compute_fluxes <- function(data,
                               group_column,
                               time_column,
                               gas_column,
                               dead_band = 0,
                               normalize_time = TRUE,
                               fit_function = ffi_fit_models,
                               ...) {

  # Convert to a data.frame so that we can be sure column-selection code
  # will work as intended; tibbles and data.tables have different behavior
  data <- as.data.frame(data)

  # Split by grouping variable
  if(is.null(group_column)) {
    x <- list(data)
  } else {
    if(!group_column %in% colnames(data)) {
      stop("There is no '", group_column, "' column in the data")
    }

    x <- split(data, data[group_column])
  }

  # Compute flux for each sample
  # passing volume and area?
  f <- function(x, ...) {
    x$.norm_time <- ffi_normalize_time(x[,time_column], normalize_time)
    x <- x[x$.norm_time >= dead_band,] # exclude dead band data
    out <- fit_function(x$.norm_time, x[,gas_column], ...)
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
