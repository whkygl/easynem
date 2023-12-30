#' @title Path to example files
#' @description This function returns the path to the example files.
#' @param path The path to the example files.
#' @return The path to the example files.
#' @export
easynem_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "easynem"))
  } else {
    system.file("extdata", path, package = "easynem", mustWork = TRUE)
  }
}
