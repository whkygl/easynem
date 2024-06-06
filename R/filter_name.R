#' filter_name
#' @description Used to filter easynem type data by name.
#' @param data easynem type data.
#' @param target tab, tax or meta, where tab represents the species abundance table, tax represents the species classification table, and meta represents the metadata table.
#' @param ... Other parameters of the filter function of the dplyr package.
#' @return An easynem object.
#' @export
filter_name <- function(data, target, ...){
  target = deparse(substitute(target))
  if(target == 'meta'){
    result = data@meta
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@meta = result
    result2 = data@tab
    result3 = result2[,-1][,colnames(result2[,-1]) %in% result[[colname]]]
    result2 = cbind(result2[,1], result3)
    data@tab = tibble::as_tibble(result2)
  } else if(target == 'tax'){
    result = data@tax
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@tax = result
    result2 = data@tab
    colname2 = colnames(result2)[1]
    result2 = result2[result2[[colname2]] %in% result[[colname]], ]
    data@tab = tibble::as_tibble(result2)
  } else if(target == 'tab'){
    result = data@tab
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@tab = tibble::as_tibble(result)
    result2 = data@tax
    colname2 = colnames(result2)[1]
    result3 = data@meta
    colname3 = colnames(result3)[1]
    result2 = result2[result2[[colname2]] %in% result[[colname]], ]
    data@tax = result2
    result3 = result3[result3[[colname3]] %in% colnames(result[,-1]), ]
    data@meta = result3
  } else{
    stop("target should be one of 'meta', 'tax' and 'tab'")
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