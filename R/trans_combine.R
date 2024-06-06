#' trans_formula
#' @description Sum meta by column.
#' @param data easynem type data.
#' @param col The name of the column to be summed.
#' @return An easynem object.
#' @export
trans_combine <- function(data, col){
  meta = data@meta
  if (!all(col %in% colnames(meta))) {
    stop("col must be present in the meta")
  }
  new_col_name <- paste(col, collapse = "_")
  meta[[new_col_name]] <- rowSums(meta[, col, drop = FALSE])
  data@meta = meta
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