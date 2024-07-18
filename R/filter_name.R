#' Filter easynem by column name and keep rows that match a condition
#'
#' The \code{filter_name()} is the extension of the \code{\link[dplyr]{filter}}
#' function for easynem type data, used to subset an easynem object, retaining
#' all rows that satisfy your conditions. This function selects one of \code{tab},
#' \code{tax} or \code{meta} in easynem for filtering. When any of the three
#' components changes, the related components will also change accordingly. To be
#' retained, the row must produce a value of \code{TRUE} for all conditions.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_filter <- nem |> filter_name(target = meta, season == "Summer")
#' ```
#'
#' @usage filter_name(data, target, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param target \code{tab}, \code{tax} or \code{meta}, where \code{tab} represents
#' the species abundance table, \code{tax} represents the species classification
#' table, and \code{meta} represents the experimental design table.
#' @param ... Other parameters of the \code{\link[dplyr]{filter}} function of the
#' dplyr package.
#'
#' @return An \code{\link{easynem-class}} data. The rows of each component are a
#' subset of the input, but appear in the same order and the columns of each
#' component are not modified.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_num}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{trans_norm}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_filter <- nem |> filter_name(target = meta, Treatments == "C4")
#' show(nem_filter)
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
