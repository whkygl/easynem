#' read_nem
#' @description Used for reading Otutab, Taxonomy, and Metadata tables.
#' Currently, this function only supports text documents in csv format.
#' @param tab Otutab table.
#' @param tax Taxonomy table.
#' @param meta Metadata table.
#' @param ... Other default parameters for read_csv function.
#' @return An easynem object.
#' @export
#' @examples
#' bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"))
#' show(bac)
read_nem <- function(tab=0, tax=0, meta=0, ...){
  .easynem = methods::new("easynem")
  if(tab != 0){
    tab = readr::read_csv(tab, show_col_types = FALSE, ...)
    colnames(tab)[1] = "OTUID"
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
  if(tax != 0){
    tax = readr::read_csv(tax, show_col_types = FALSE, ...)
    colnames(tax)[1] = "OTUID"
    .easynem@tax = tax
    if("Nematoda" %in% .easynem@tax$Phylum) {
      basis = basis1[!(is.na(basis1[,4]) & is.na(basis1[,5])), ]
      basis = basis[,-c(2,3,6)]
      names(basis) = names(genus1)[1:3]
      intername = intersect(genus1$Genus, basis$Genus)
      basis_ = basis |> dplyr::filter(!Genus %in% intername)
      basis_[,4:27] = NA
      names(basis_)[4:27] = names(genus1)[4:27]
      genus = rbind(genus1, basis_)
      genus = dplyr::distinct(genus)
      hehe = dplyr::left_join(.easynem@tax,genus,by = "Genus")
      na_rows = apply(hehe[, 9:34], 1, function(row) all(is.na(row)))
      hehe_na_rows = hehe[na_rows, ]
      hehe_na_rows = hehe_na_rows[,1:8]
      hehe2 = dplyr::left_join(hehe_na_rows,family1,by = "Family")
      hehe2 = hehe2[,-c(11,12)]
      hehe = hehe[!na_rows,]
      names(hehe2) = names(hehe)
      hehe_all = rbind(hehe,hehe2)
      .easynem@tax = hehe_all
    }
  } else {
    warning("Taxonomy has not been imported yet\n")
  }
  if(meta != 0 && is.numeric(tab)){
    meta = readr::read_csv(meta, show_col_types = FALSE, ...)
    colnames(meta)[1] = "SampleID"
    .easynem@meta = meta
  } else if(meta !=0 && !is.numeric(tab)){
    meta = readr::read_csv(meta, show_col_types = FALSE, ...)
    colnames(meta)[1] = "SampleID"
    tab = .easynem@meta
    meta = meta |> dplyr::left_join(tab, by = "SampleID")
    .easynem@meta = meta
  } else {
    warning("Metadata has not been imported yet\n")
  }
  return(.easynem)
}