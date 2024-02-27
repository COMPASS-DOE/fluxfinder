# qaqc.R


#' Generate a QA/QC document
#'
#' @param output_file Name of the output file
#' @param output_dir Name of the output directory; default is current working directory
#' @return The path of the output file
#' @export
#'
#' @examples
#' x <- qaqc()
#' file.remove(x) # clean up
qaqc <- function(output_file = "qaqc.html", output_dir = getwd()) {
  f <- system.file("qaqc.Rmd", package = "whattheflux")
  if(requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render(f, output_file = output_file, output_dir = output_dir)
  } else {
    stop("To run this function, please install the rmarkdown package")
  }
}
