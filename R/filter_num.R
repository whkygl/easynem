#' Filter easynem's tab by discovery rate or abundance
#'
#' The \code{filter_num()} is used to filter the rows of the easynem \code{tab}
#' by abundance or discovery rate. If \code{num>1}, filter by abundance, \code{num}
#' is the lowest abundance of the \code{tab}; if \code{num<1}, filter by discovery
#' rate, \code{num} is the lowest discovery rate of the \code{tab}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_filter <- nem |> filter_num(target = meta, num = 0.85)
#' ```
#' ```
#' nem_filter <- nem |> filter_num(target = meta, num = 500)
#' ```
#'
#' @usage filter_num(data, num)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param num Filter threshold value. If \code{num>1}, filter by abundance, \code{num}
#' is the lowest abundance of the \code{tab}; if \code{num<1}, filter by discovery
#' rate, \code{num} is the lowest discovery rate of the \code{tab}.
#'
#' @return An \code{\link{easynem-class}} data.The results of \code{tab}, \code{tax},
#' and \code{meta} are the retention values after filtering the \code{tab} by abundance
#' or discovery rate.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{trans_norm}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_filter <- nem |> filter_num(num = 0.9)
#' show(nem_filter)
#' nem_filter <- nem |> filter_num(num = 1000)
#' show(nem_filter)
filter_num  <- function(data, num){
  result = data@tab
  colname = colnames(result)[1]
  result2 = data@tax
  colname2 = colnames(result2)[1]
  if(num >= 0 && num < 1){
    result = result[rowSums(result[,-1] != 0) / ncol(result[,-1]) >= num, ]
  } else if(num >= 1){
    result = result[rowSums(result[,-1]) >= num, ]
  } else {
    stop("num should be a positive number")
  }
  result2 = result2[result2[[colname2]] %in% result[[colname]], ]
  data@tab = result
  data@tax = result2
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
