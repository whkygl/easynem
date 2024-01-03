#' trans_formula
#' @description Convert data according to a formula.
#' @param data easynem type data.
#' @param var Variable name to be converted.
#' @param formu Formula parameters for data conversion.
#' @param ... Other parameters of the vegan package decostand function.
#' @return An easynem object.
#' @export
trans_formula <- function(data, var, formu, ...){
  var = deparse(substitute(var))
  resultmeta = data@meta
  suppressWarnings(resultmeta[[var]] <-  eval(formu[[2]], list(x = data@meta[[var]]),environment(formu)))
  data@meta = resultmeta
  return(data)
}