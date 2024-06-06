#' filter_num
#' @description Used to filter species whose quantity or discovery rate is higher than num. If num is greater than 1, filter by quantity; if num is less than 1, filter by discovery rate.
#' @param data easynem type data.
#' @param num Filter threshold value.
#' @param ... Other parameters.
#' @return An easynem object.
#' @export
filter_num  <- function(data, num, ...){
  result = data@tab
  colname = colnames(result)[1]
  result2 = data@tax
  colname2 = colnames(result2)[1]
  if(num >= 0 && num < 1){
    result = result[rowSums(result[,-1] != 0) / ncol(result[,-1]) >= num, ]
  } else if(num >= 1){
    result = result[rowSums(result[,-1]) >= num, ]
  } else {
    stop("num should be a positive number")
  }
  result2 = result2[result2[[colname2]] %in% result[[colname]], ]
  data@tab = result
  data@tax = result2
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