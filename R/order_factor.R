#' order_factor
#' @description Meta attributes of easynem grouping factors in order to rearrangement.
#' @param data easynem type data.
#' @param group Selection of meta columns.
#' @param order Order of factors.
#' @return An easynem object.
#' @export
order_factor <- function(data, group, order){
    meta = data@meta
    group = deparse(substitute(group))
    meta[[group]] = factor(meta[[group]], levels = order)
    data@meta = meta
    return(data)
}