################################################################################
#' The main experiment-level class for easynem data
#'
#' Integrate the nematode abundance table, nematode classification table, and
#' experimental design table into an \code{easynem-class}, which makes it easier to
#' filter and manage nematode data, and easier to link to the nematode database
#' and conduct subsequent analysis.
#'
#' Users can read data via \code{\link{read_nem}} or \code{\link{read_nem2}}.
#' When there are missing slots in easynem, the system will issue a warning, but
#' this will not affect subsequent analysis.
#'
#'@slot tab A single object of nematode abundance table.
#'@slot tax A single object of nematode classification table.
#'@slot meta A single object of experimental design table.
#'
#'@seealso
#' The constructor, \code{\link{read_nem}} for reading csv files and
#' \code{\link{read_nem2}} for reading tibble type data.
#'
#' @name easynem-class
#' @rdname easynem-class
#' @exportClass easynem
methods::setClass("easynem",
                  slots = list(
                    tab = "data.frame",
                    tax = "data.frame",
                    meta = "data.frame"
                  ))
methods::setMethod("show", "easynem", function(object){
  cat("This is an easynem object\n")
  cat("The otutab is:\n")
  print(object@tab)
  cat("The taxonomy is:\n")
  print(object@tax)
  cat("The metadata is:\n")
  print(object@meta)
})
