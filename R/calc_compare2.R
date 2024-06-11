#' calc_compare2
#' @description Differences between groups were compared.
#' @param data easynem type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @param y The dependent variable.
#' @param method The method of difference comparison.
#' @param ... Other parameters for method functions.
#' @return An compare2 object.
#' @export
calc_compare2 <- function(data, .group1, .group2, y, method, ...){
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  y = deparse(substitute(y))
  result = method(data, .group1, .group2, y, ...)
  return(result)
}