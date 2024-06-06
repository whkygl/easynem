#' trans_name
#' @description Used to sum species abundance tables grouped by taxonomy table.
#' @param data easynem type data.
#' @param taxonomy Species classification name.
#' @param ... Other parameters.
#' @return An easynem object.
#' @export
trans_name <- function(data, taxonomy, ...){
  nametax = deparse(substitute(taxonomy))
  tax = data@tax
  tab = data@tab
  intersect1 = intersect(tax[[1]], tab[[1]])
  tax = tax[tax[[1]] %in% intersect1, ]
  tab = tab[tab[[1]] %in% intersect1, ]
  if(nametax %in% colnames(data@tax)){
    if(nametax == colnames(data@tax)[1]){
      tax = tax[match(tab[[1]], tax[[1]]), ]
    } else {
      tax = tax[match(tab[[1]], tax[[1]]), ]
      tab[[1]] = tax[[nametax]]
      colnames(tab)[1] = nametax
      tab = tab |> dplyr::group_by(!!rlang::sym(nametax)) |> dplyr::summarise_all(sum)
    }
  } else {
    stop("Please check that the taxonomy name are correct")
  }
  data@tax = tax
  data@tab = tab
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