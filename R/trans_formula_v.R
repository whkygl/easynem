#' Formula conversion for easynem's meta (Vectorization)
#'
#' The \code{trans_formula_v()} is used to convert the formula of easynem meta.
#' Formula transformation is sometimes necessary in nematode community analysis.
#' For example, to ensure that the data is normally distributed, it is often
#' necessary to perform \code{ln(x+1)} transformation or other forms of formula
#' transformation on nematode abundance. This function can transfer vectors to
#' achieve multi-variable formula conversion. For a univariate simplified version
#' of this function, see \code{\link{trans_formula}}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_formula_v(colnames(resultmeta)[5:10], ~log(x+1))
#' ```
#'
#' @usage trans_formula_v(data, var, formu)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param var Vectorized variable names for formula conversion.
#' @param formu Formula parameters for data conversion. Such as \code{~log(x+1)}.
#'
#' @return An \code{\link{easynem-class}} data that stores the result of formula
#' conversion.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{filter_num}}, \code{\link{trans_formula}},
#' \code{\link{trans_name}}, \code{\link{trans_norm}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |> trans_formula_v(nem@tab$OTUID, ~log(x+1))
#' show(nem_trans)
trans_formula_v <- function(data, var, formu){
  # resultmeta = meta@meta
  # var = colnames(resultmeta)[5:10]
  # formu = ~1/x
  resultmeta = data@meta
  resultmeta[var] <- eval(formu[[2]], list(x = resultmeta[var]), environment(formu))
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
  tab[var] = meta[var]
  meta = tibble::as_tibble(meta)
  tab = tibble::as_tibble(tab)
  data@meta = meta
  data@tab = tab
  return(data)
}
