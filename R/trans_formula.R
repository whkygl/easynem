#' Formula conversion for easynem's meta
#'
#' The \code{trans_formula()} is used to convert the formula of easynem meta.
#' Formula transformation is sometimes necessary in nematode community analysis.
#' For example, to ensure that the data is normally distributed, it is often
#' necessary to perform \code{ln(x+1)} transformation or other forms of formula
#' transformation on nematode abundance. This function only works on a single
#' variable. For a vectorized variant of this function, see \code{\link{trans_formula_v}}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_trans <- nem |> trans_formula(Mesorhabditis, ~log(x+1))
#' ```
#'
#' @usage trans_formula(data, var, formu)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param var Variable name to be converted.
#' @param formu Formula parameters for data conversion. Such as \code{~log(x+1)}.
#'
#' @return An \code{\link{easynem-class}} data that stores the result of formula
#' conversion.
#'
#' @seealso
#' Other functions in this package for filtering and transforming data sets:
#' \code{\link{filter_name}}, \code{\link{filter_num}}, \code{\link{trans_formula_v}},
#' \code{\link{trans_name}}, \code{\link{trans_norm}}, \code{\link{trans_rare}},
#' \code{\link{trans_combine}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_trans <- nem |> trans_formula(Mesorhabditis, ~log(x+1))
#' show(nem_trans)
trans_formula <- function(data, var, formu){
  var = deparse(substitute(var))
  resultmeta = data@meta
  suppressWarnings(resultmeta[[var]] <-  eval(formu[[2]], list(x = data@meta[[var]]),environment(formu)))
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
