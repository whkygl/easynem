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
  if (method == "percent") {
    data@tab[,-1] = vegan::decostand(data@tab[,-1], method = "total", MARGIN = MARGIN, ...) * 100
  } else {
    data@tab[,-1] = vegan::decostand(data@tab[,-1], method = method, MARGIN = MARGIN, ...)
  }
  tab = as.data.frame(data@tab)
  rownames(tab) = tab[,1]
  tab = tab[,-1]
  tab = t(tab)
  tab = as.data.frame(tab)
  tab$SampleID = rownames(tab)
  tab = tab[,c(ncol(tab), 1:(ncol(tab)-1))]
  colnames(tab)[1] = "SampleID"
  meta = data@meta
  colnames(meta)[1] = "SampleID"
  if(any(names(tab[,-1]) %in% names(meta[,-1]))){
    dif = dplyr::setdiff(names(meta), names(tab))
    meta = meta[,c("SampleID", dif)]
    meta = merge(meta, tab, by = "SampleID")
  } else {
    meta = merge(meta, tab, by = "SampleID")
  }
  meta = tibble::as_tibble(meta)
  data@meta = meta
  return(data)
}