#' Normalizing the nematode abundance table
#'
#' The \code{trans_norm()} is an extension of the \code{\link[vegan]{decostand}}
#' function of the vegan package for \code{\link{easynem-class}} data, which is
#' used to standardize the nematode abundance table to reduce the order of magnitude
#' differences of nematodes in each treatment.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_norm(method = total)
#' ```
#'
#' @usage trans_norm(data, method, MARGIN = 2, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param method Standardization method. For details, refer to the
#' \code{\link[vegan]{decostand}} function of the vegan package.
#' @param MARGIN Margin, \code{1 = rows}, and \code{2 = columns} of easynem's tab.
#' Default \code{MARGIN = 2}.
#' @param ... Other parameters of the \code{\link[vegan]{decostand}} function of
#' the vegan package.
#'
#' @return A normalized \code{\link{easynem-class}} data.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{filter_num}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |> trans_norm(method = total)
#' colSums(nem_trans@tab[,-1])
#' nem_trans <- nem |> trans_norm(method = percent)
#' colSums(nem_trans@tab[,-1])
trans_norm <- function(data, method, MARGIN = 2, ...){
  method = deparse(substitute(method))
  if (method == "percent") {
    data@tab[,-1] = vegan::decostand(data@tab[,-1], method = "total", MARGIN = MARGIN, ...) * 100
  } else {
    data@tab[,-1] = vegan::decostand(data@tab[,-1], method = method, MARGIN = MARGIN, ...)
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
