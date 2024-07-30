devtools::install_github("whkygl/easynem")
library(easynem)
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' A S4 class to store the linear regression analysis results (single factor)
#'
#' The \code{lme-class} is used to store the results of linear regression analysis.
#'
#' @slot result A data frame for storing the results of linear regression analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_lm}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name lme-class
#' @rdname lme-class
#' @exportClass lme
methods::setClass("lme",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY"
                  ))
methods::setMethod("show", "lme", function(object){
  cat("This is an lme object\n")
  cat("The Linear regression analysis results is:\n")
  print(object@result)
})
#' Linear regression analysis of easynem-class (single factor)
#'
#' The \code{calc_lm()} function is used for linear regression analysis of
#' \code{\link{easynem-class}}. Note: Both the horizontal and vertical coordinates of this
#' function must be continuous variables.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_lm <- nem |> calc_lm(con_crop, x = SOC, y = pH)
#' ```
#' @usage calc_lm(data, group, x, y, ...)
#'
#' @param data An \code{\link{easynem-class}} object.
#' @param group The group variable.
#' @param x X-axis.
#' @param y Y-axis.
#' @param ... Other parameters of the \code{\link[stats]{lm}} function.
#'
#' @return Returns an \code{\link{lme-class}} object storing the results of a
#' linear regression analysis.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf2}},
#' \code{\link{calc_mf}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}, \code{\link{calc_lm2}}
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |>
#'              calc_alpha() |>
#'              calc_nemindex() |>
#'              calc_lm(group = Treatments,
#'                      x = Chao1,
#'                      y = TotalBiomass)
#' nem_index
calc_lm <- function(data, group, x, y, ...){
  # data = nem_index
  # group = "Treatments"
  # x = "Chao1"
  # y = "TotalBiomass"
  group = deparse(substitute(group))
  x = deparse(substitute(x))
  y = deparse(substitute(y))
  .lm = methods::new("lme")
  meta = tibble::as_tibble(data@meta)
  meta = meta[,c(names(meta)[1], group, x, y)]
  formu <- stats::as.formula(paste0(y, " ~ ", x))
  fit = stats::lm(formu, data = meta, ...)
  .lm@result = summary(fit)
  .lm@meta = tibble::as_tibble(meta)
  return(.lm)
}
