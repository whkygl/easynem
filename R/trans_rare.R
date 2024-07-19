#' Randomly rarefied OTU or ASV tables of nematodes for amplicon sequencing data
#'
#' The \code{trans_rare()} is an extension of the \code{\link[vegan]{rrarefy}}
#' function of the vegan package for \code{\link{easynem-class}} data, which is
#' used to randomly rarefied OTU or ASV tables of nematodes for amplicon sequencing
#' data. The default is to rare according to the minimum abundance of nematode
#' in each treatment.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_rare(1500)
#' ```
#'
#' @usage trans_rare(data, sample = 0, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param sample Subsample size for rarefying community. The default \code{sample = 0},
#' the minimum abundance is used for rarefied OTU or ASV tables of nematodes.
#' @param ... Other parameters of the \code{\link[vegan]{rrarefy}} function of
#' the vegan package.
#'
#' @return A rarefied \code{\link{easynem-class}} data.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{filter_num}}, \code{\link{trans_norm}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |> trans_rare()
#' colSums(nem_trans@tab[,-1])
#' nem_trans <- nem |> trans_rare(1500)
#' colSums(nem_trans@tab[,-1])
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
