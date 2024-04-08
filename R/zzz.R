# zzz.R - miscellany

#' Check if fluxfinder.quiet option is TRUE
#' @return TRUE or FALSE
#' @keywords internal
ffi_isquiet <- function() {
  getOption("fluxfinder.quiet", default = FALSE)
}

ffi_message <- function(...) {
  if (ffi_isquiet()) {
    return()
  }
  message(...)
}
