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
qaqc <- function(output_file = "qaqc.html",
                 output_dir = getwd(),
                 open_output = TRUE) {
  f <- system.file("qaqc.Rmd", package = "whattheflux")
  if(requireNamespace("rmarkdown", quietly = TRUE)) {
    fout <- rmarkdown::render(f, output_file = output_file, output_dir = output_dir)
    if(open_output) browseURL(paste0('file://', fout))
    return(fout)
  } else {
    stop("To run this function, please install the rmarkdown package")
  }
}
