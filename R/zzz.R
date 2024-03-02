# zzz.R - miscellany

#' Check if whattheflux.quiet option is TRUE
#' @return TRUE or FALSE
#' @keywords internal
wtf_isquiet <- function() {
  getOption("whattheflux.quiet", default = FALSE)
}

wtf_message <- function(...) {
  if (wtf_isquiet()) {
    return()
  }
  message(...)
}
