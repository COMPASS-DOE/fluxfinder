# zzz.R - miscellany

wtf_message <- function(...) {
  if (getOption("whattheflux.quiet", default = FALSE)) {
    return()
  }
  message(...)
}
