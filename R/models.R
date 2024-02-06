# models.R

#' Fit various models to gas concentration data
#'
#' @param time Relative time of observation (typically seconds), numeric
#' @param conc Greenhouse gas concentration, numeric
#' @param quiet Be quiet? Logical
#' @return A wide-form \code{\link{data.frame}} with fit statistics for linear,
#' robust linear, and polynomial models. By default, extensive details are
#' provided only for the linear fi; for robust linear and polynomial, only
#' the slope and R2 are returned, respectively, as these are intended for QA/QC.
#' @details If a linear model cannot be fit, NULL is returned. If the robust
#' linear and/or polynomial models cannot be fit, then \code{NA} is returned
#' for their particular statistics.
#' @importFrom broom glance tidy
#' @importFrom MASS rlm
#' @importFrom stats lm coefficients
#' @export
#' @examples
#' # Toy data
#' wtf_fit_models(cars$speed, cars$dist)
#' # Real data
#' f <- system.file("extdata/TG10-01087.data", package = "whattheflux")
#' dat <- wtf_read_LI7810(f)[1:75,] # manually isolate first observation
#' dat$SECONDS <- dat$SECONDS - min(dat$SECONDS) # normalize time to start at 0
#' plot(dat$SECONDS, dat$CO2)
#' wtf_fit_models(dat$SECONDS, dat$CO2)
wtf_fit_models <- function(time, conc, quiet = FALSE) {
  # Basic linear model
  try(mod <- lm(conc ~ time))
  if(!exists("mod")) {
    if(!quiet) warning("Could not fit linear model")
    return(NULL)
  }

  # Linear model overall metrics
  model_stats <- glance(mod)

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
    if(!quiet) warning("Could not fit robust linear model")
    NA_real_
  })

  # Add polynomial regression R2 as a QA/QC check
  slope_stats$r.squared_poly <- tryCatch({
    poly <- lm(conc ~ poly(time, 3))
    summary(poly)$r.squared
  },
  error = function(e) {
    if(!quiet) warning("Could not fit polynomial model")
    NA_real_
  })

  res <- cbind(model_stats, slope_stats, intercept_stats)

  # Round to a sensible number of digits and return
  numerics <- sapply(res, is.numeric)
  res[numerics] <- round(res[numerics], 3)
  return(res)
}
