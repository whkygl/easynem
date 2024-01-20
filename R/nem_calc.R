#' calc
#' @description For microbial or nematode community calculations.
#' @param data easynem type data.
#' @param f Function parameters for microbial or nematode community calculations.
#' @param ... Other parameters.
#' @return easynem or other data types.
#' @export
nem_calc <- function(data, f, ...){
  result = f(data, ...)
  return(result)
}