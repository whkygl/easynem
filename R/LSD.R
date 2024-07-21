#' Multiple comparisons, "Least significant difference" and Adjust P-values (single factor)
#'
#' The \code{LSD()} is used to perform "Least significant difference" for
#' grouped data and create \code{\link{compare-class}}. This function is only
#' applicable to single factor analysis, see \code{\link{LSD2}} for a
#' two factor version of the function.
#'
#' To facilitate code interpretation, It is recommended to use this function in
#' conjunction with the \code{\link{calc_compare}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare(.group = con_crop, y = pH, method = LSD)
#' ```
#'
#' @usage LSD(data, .group, y, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group Grouping variables.
#' @param y Dependent variable (numeric data).
#' @param ... Other parameters for \code{\link[agricolae]{LSD.test}}.
#'
#' @return An \code{\link{compare-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest2}},
#' \code{\link{TTest}}, \code{\link{WilcoxTest2}}, \code{\link{WilcoxTest}},
#' \code{\link{KruskalTest2}}, \code{\link{KruskalTest}}, \code{\link{LSD2}}, \code{\link{HSD}},
#' \code{\link{HSD2}}.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_test <- nem |>
#'             calc_compare(.group = Treatments,
#'               y = Mesorhabditis,
#'               method = LSD)
#' nem_test
LSD <- function(data, .group, y, ...){
  .compare = methods::new("compare")
  meta = as.data.frame(data@meta)
  meta = meta[, c(names(meta)[1], .group, y)]
  row.names(meta) = meta[,1]
  formula_str <- paste(y, "~", .group)
  formula <- stats::as.formula(formula_str)
  fit = stats::aov(formula, data = meta, ...)
  .compare@meta = meta
  .compare@result$`Fit an Analysis of Variance Model` = summary(fit)
  .compare@temp = c("LSD")
  lsd = agricolae::LSD.test(fit, .group, ...)
  .compare@result$LSD = lsd
  return(.compare)
}
