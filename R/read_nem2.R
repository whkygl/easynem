#' Build easynem-class objects from their tibble type object
#'
#' \code{read_nem2()} is a constructor method. This is the main method suggested
#' for constructing an experiment-level (\code{\link{easynem-class}}) object
#' from its tibble type object (component data: \code{tab}, \code{tax}, \code{meta}).
#'
#' @usage read_nem2(tab = 0, tax = 0, meta = 0, ...)
#'
#' @param tab Nematode abundance table.
#' @param tax Nematode abundance table.
#' @param meta Experimental design table.
#' @param ... Other default parameters for \code{\link[readr]{read_csv}} function.
#'
#' @return An easynem object. The components in the class are interconnected to
#' facilitate the subsequent screening and management of nematode data. When this
#' class is generated, it will automatically check whether there is nematode
#' information in the species classification table. If not, it will not be
#' associated with the nematode database.
#'
#' @seealso \code{\link{read_nem}}
#' @export
#' @examples
#' easynem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' show(easynem)
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
      na_rows = apply(hehe[, (length(tax)+1):(length(tax)+26)], 1, function(row) all(is.na(row)))
      hehe_na_rows = hehe[na_rows, ]
      hehe_na_rows = hehe_na_rows[,1:length(tax)]
      hehe2 = dplyr::left_join(hehe_na_rows,family1,by = "Family")
      hehe2 = hehe2[,-c(length(tax)+3, length(tax)+4)]
      hehe = hehe[!na_rows,]
      names(hehe2) = names(hehe)
      hehe_all = rbind(hehe,hehe2)
      .easynem@tax = hehe_all
    }
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
