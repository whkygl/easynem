#' nem_trans
#' @description Used to convert and filter easynem type data.
#' @param data easynem type data.
#' @param f Function parameters for data filtering and transformation.
#' @param ... Other parameters.
#' @return An easynem object.
#' @export
nem_trans <- function(data, f, ...){
  result = f(data, ...)
  return(result)
}