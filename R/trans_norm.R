#' trans_norm
#' @description Standardize species numbers to percentages or other.
#' @param data easynem type data.
#' @param method Methods used in standardization.
#' @param MARGIN The dimension to standardize. 1 for rows, 2 for columns.
#' @param ... Other parameters of the vegan package decostand function.
#' @return An easynem object.
#' @export
trans_norm <- function(data, method, MARGIN = 2, ...){
  method = deparse(substitute(method))
  data@tab[,-1] = vegan::decostand(data@tab[,-1], method = method, MARGIN = MARGIN, ...)
  return(data)
}