# qaqc.R


#' Generate a QA/QC document
#'
#' @param output_file Name of the output file
#' @param output_dir Name of the output directory; default is current working directory
#' @param open_output Automatically open the output HTML file?
#' @importFrom utils browseURL
#' @return The path of the output file
#' @export
#'
#' @examples
#' x <- qaqc(open_output = FALSE)
#' file.remove(x) # clean up
wtf_qaqc <- function(output_file = "qaqc.html",
                 output_dir = getwd(),
                 open_output = TRUE) {
  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("To run this function, please install the rmarkdown package")
  }

  f <- system.file("qaqc.Rmd", package = "whattheflux")
  fout <- rmarkdown::render(f,
                            output_file = output_file,
                            output_dir = output_dir,
                            quiet = wtf_isquiet())
  if(open_output) browseURL(paste0('file://', fout))
  invisible(fout)
}
