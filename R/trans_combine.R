#' Merge multiple columns of easynem's meta
#'
#' The \code{trans_combine()} is used for the special case of merging columns in
#' easynem's meta. For example, \code{Cp35%} (the sum of percentages from \code{Cp3}
#' to \code{Cp5}) is often used in nematode community analysis. This function can
#' quickly merge \code{Cp3} to \code{Cp5}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_combine(c("3", "4", "5"))
#' ```
#'
#' @usage trans_combine(data, col)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param col The name of the column to be summed.
#'
#' @return An \code{\link{easynem-class}} data.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{trans_formula}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{filter_num}}, \code{\link{trans_norm}},
#' \code{\link{trans_rare}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |>
#'              trans_name(cp_value) |>
#'              trans_norm(method = percent) |>
#'              trans_combine(c("3", "4", "5"))
#' show(nem_trans)
#' nem_trans@meta$`3_4_5`
trans_combine <- function(data, col){
  meta = data@meta
  if (!all(col %in% colnames(meta))) {
    stop("col must be present in the meta")
  }
  new_col_name <- paste(col, collapse = "_")
  meta[[new_col_name]] <- rowSums(meta[, col, drop = FALSE])
  data@meta = meta
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
