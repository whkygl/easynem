methods::setClass("lme2",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY"
                  ))
methods::setMethod("show", "lme2", function(object){
  cat("This is an lme2 object\n")
  cat("The Linear regression analysis results is:\n")
  print(object@result)
})
#' Linear regression analysis of easynem-class (two-factor)
#'
#' The \code{calc_lm2()} function is used for linear regression analysis of
#' \code{\link{easynem-class}}. Note: Both the horizontal and vertical coordinates of this
#' function must be continuous variables.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_lm <- nem |> calc_lm2(con_crop, season, x = SOC, y = pH)
#' ```
#' @usage calc_lm2(data, group1, group2, x, y, ...)
#'
#' @param data An \code{\link{easynem-class}} object.
#' @param group1 The group variable factor 1.
#' @param group2 The group variable factor 2.
#' @param x X-axis.
#' @param y Y-axis.
#' @param ... Other parameters of the \code{\link[stats]{lm}} function.
#'
#' @return Returns an \code{\link{lme2-class}} object storing the results of a
#' linear regression analysis.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf2}},
#' \code{\link{calc_mf}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}, \code{\link{calc_lm}}.
#'
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_lm <- nem |> calc_lm2(con_crop, season, x = pH, y = Fe)
calc_lm2 <- function(data, group1, group2, x, y, ...){
  # data = nem
  # group1 = "con_crop"
  # group2 = "season"
  # x = "pH"
  # y = "Fe"
  group1 = deparse(substitute(group1))
  group2 = deparse(substitute(group2))
  x = deparse(substitute(x))
  y = deparse(substitute(y))
  .lm = methods::new("lme2")
  meta = tibble::as_tibble(data@meta)
  meta = meta[,c(names(meta)[1], group1, group2, x, y)]
  formu <- stats::as.formula(paste0(y, " ~ ", x))
  results_list <- list()
  meta |>
    group_by(!!rlang::sym(group2)) |>
    dplyr::do({
      model <- stats::lm(formu, data = .)
      result <- summary(model)
      results_list[[as.character(unique(.[[group2]]))]] <<- result
      data.frame()
    })
  .lm@result = results_list
  .lm@meta = tibble::as_tibble(meta)
  return(.lm)
}
