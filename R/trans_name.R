#' Reorganize the easynem's tab by taxonomic name
#'
#' The \code{trans_name()} is used to re-summarize the nematode abundance table
#' by nematode taxonomy table.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_name(Family)
#' ```
#'
#' @section Feedings:
#'
#' Since the nematode taxonomy table is automatically associated with the nematode
#' database (\code{\link{nem_database}}) including \code{feeding} and \code{cp_value}
#' when reading data through \code{\link{read_nem}} or \code{\link{read_nem2}},
#' \code{feeding} can also be passed as a parameter to \code{trans_name()}. The
#' corresponding relationship between the feeding value and the actual nematode
#' feeding habits is as follows:
#'
#' * `feeding = 1`, plant feeding
#' * `feeding = 2`, fungal hyphal feeding
#' * `feeding = 3`, bacterial feeding
#' * `feeding = 4`, substrate ingestion
#' * `feeding = 5`, predation (including specialist predators of nematodes)
#' * `feeding = 6`, eucaryote feeding
#' * `feeding = 7`, dispersal stages or animal parasites
#' * `feeding = 8`, omnivory (including general predators of nematodes)
#'
#' @usage trans_name(data, taxonomy)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param taxonomy Nematode taxonomic name or other nematode attributes.
#'
#' @return A reclassified and aggregated \code{\link{easynem-class}}.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{filter_num}}, \code{\link{trans_norm}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |> trans_name(Family)
#' show(nem_trans)
#' nem_trans <- nem |> trans_name(feeding)
#' show(nem_trans)
trans_name <- function(data, taxonomy){
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
  if(any(names(tab)[-1] %in% names(meta)[-1])){
    char_columns <- sapply(meta, is.character)
    meta = meta[,char_columns]
    meta = merge(meta, tab, by = "SampleID")
  } else {
    meta = merge(meta, tab, by = "SampleID")
  }
  meta = tibble::as_tibble(meta)
  data@meta = meta
  return(data)
}
