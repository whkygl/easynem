#' calc_compare
#' @description Differences between groups were compared.
#' @param data easynem type data.
#' @param .group The group variable.
#' @param y The dependent variable.
#' @param method The method of difference comparison.
#' @param ... Other parameters for method functions.
#' @return An compare object.
#' @export
calc_compare <- function(data, .group, y, method, ...){
    .group = deparse(substitute(.group))
    y = deparse(substitute(y))
    result = method(data, .group, y, ...)
    return(result)
}
