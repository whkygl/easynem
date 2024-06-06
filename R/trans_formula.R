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