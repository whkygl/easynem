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
#' read_nem
#' @description Used for reading Otutab, Taxonomy, and Metadata tables.
#' Currently, this function only supports text documents in csv format.
#' @param tab Otutab table.
#' @param tax Taxonomy table.
#' @param meta Metadata table.
#' @param ... Other default parameters for read_csv function.
#' @return An easynem object.
#' @export
#' @examples
#' bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"))
#' show(bac)
read_nem <- function(tab=0, tax=0, meta=0, ...){
  .easynem = methods::new("easynem")
  if(tab != 0){
    tab = readr::read_csv(tab, show_col_types = FALSE, ...)
    .easynem@tab = tab
  } else {
    warning("Otutab has not been imported yet")
  }
  if(tax != 0){
    tax = readr::read_csv(tax, show_col_types = FALSE, ...)
    .easynem@tax = tax
  } else {
    warning("Taxonomy has not been imported yet")
  }
  if(meta != 0){
    meta = readr::read_csv(meta, show_col_types = FALSE, ...)
    .easynem@meta = meta
  } else {
    warning("Metadata has not been imported yet")
  }
  return(.easynem)
}