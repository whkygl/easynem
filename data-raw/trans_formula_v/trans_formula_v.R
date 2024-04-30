#' trans_formula_v
#' @description Convert data according to a formula.
#' @param data easynem type data.
#' @param var Variable name to be converted.
#' @param formu Formula parameters for data conversion.
#' @return An easynem object.
#' @export
trans_formula_v <- function(data, var, formu){
  # resultmeta = meta@meta
  # var = colnames(resultmeta)[5:10]
  # formu = ~1/x
  resultmeta = data@meta
  resultmeta[var] <- eval(formu[[2]], list(x = resultmeta[var]), environment(formu))
  data@meta = resultmeta
  return(data)
}