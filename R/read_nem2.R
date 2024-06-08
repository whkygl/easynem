#' read_nem2
#' @description Used for reading Otutab, Taxonomy, and Metadata tables.
#' Currently, this function only supports text documents in dataframe format.
#' @param tab Otutab table.
#' @param tax Taxonomy table.
#' @param meta Metadata table.
#' @param ... Other default parameters for read_nem2 function.
#' @return An easynem object.
#' @export
read_nem2 <- function(tab=0, tax=0, meta=0, ...){
  .easynem = methods::new("easynem")
  if(!is.numeric(tab)){
    tab = tab
    colnames(tab)[1] = "OTUID"
    tab = tibble::as_tibble(tab)
    .easynem@tab = tab
    tab = as.data.frame(tab)
    rownames(tab) = tab[,1]
    tab = tab[,-1]
    tab = t(tab)
    tab = as.data.frame(tab)
    tab$SampleID = rownames(tab)
    tab = tab[,c(ncol(tab), 1:(ncol(tab)-1))]
    colnames(tab)[1] = "SampleID"
    tab = tibble::as_tibble(tab)
    .easynem@meta = tab
  } else {
    warning("Otutab has not been imported yet\n")
  }
  if(!is.numeric(tax)){
    tax = tax
    colnames(tax)[1] = "OTUID"
    tax = dplyr::as_tibble(tax)
    .easynem@tax = tax
  } else {
    warning("Taxonomy has not been imported yet\n")
  }
  if(!is.numeric(meta) && is.numeric(tab)){
    meta = meta
    colnames(meta)[1] = "SampleID"
    meta = dplyr::as_tibble(meta)
    .easynem@meta = meta
  } else if(!is.numeric(meta) && !is.numeric(tab)){
    meta = meta
    colnames(meta)[1] = "SampleID"
    tab = .easynem@meta
    meta = meta |> dplyr::left_join(tab, by = "SampleID")
    meta = dplyr::as_tibble(meta)
    .easynem@meta = meta
  } else {
    warning("Metadata has not been imported yet\n")
  }
  return(.easynem)
}