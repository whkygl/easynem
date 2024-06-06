#' trans_rare
#' @description Used to rare the species abundance table according to the number of occurrences in the sample. The default is to rare according to the minimum value.
#' @param data easynem type data.
#' @param sample The number of occurrences in the sample. If sample = 0, the minimum value is used by default.
#' @param ... Other parameters of the vegan package rrarefy function.
#' @return An easynem object.
#' @export
trans_rare <- function(data, sample = 0, ...){
  # data = hehe
  # sample = 0
  resulttab = data@tab
  resulttax = data@tax
  resultmeta = data@meta
  nametab = colnames(data@tab)[1]
  nametax = colnames(data@tax)[1]
  namemeta = colnames(data@meta)[1]
  data@tab = as.data.frame(data@tab)
  rownames(data@tab) <- data@tab[[1]]
  if (sample == 0) {
    sample = min(colSums(data@tab[,-1]))
    data@tab = tibble::as_tibble(tibble::rownames_to_column(as.data.frame(t(vegan::rrarefy(t(data@tab[ ,-1]), sample, ...))), var = nametab))
  } else if (sample > 0){
    suppressWarnings(result <- as.data.frame(t(vegan::rrarefy(t(data@tab[ ,-1]), sample))), ...)
    warning("All samples with less than ", sample, " species will be deleted\n")
    data@tab <- tibble::as_tibble(tibble::rownames_to_column(result[ , colSums(result) >= sample], var = nametab))
  } else {
    stop("sample should be a positive number")
  }
  resulttax = resulttax[resulttax[[nametax]] %in% data@tab[[nametab]], ]
  data@tax = resulttax
  resultmeta = resultmeta[resultmeta[[namemeta]] %in% colnames(data@tab[,-1]), ]
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