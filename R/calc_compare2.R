#' Multiple comparisons between treatments (two-factor)
#'
#' The \code{calc_compare2} is used for multiple comparisons between different
#' treatments and create \code{\link{compare2-class}}. This function is only
#' applicable to two-factor analysis, see \code{\link{calc_compare}} for a
#' single factor version of the function.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_compare <- nem |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
#' ```
#'
#' @usage calc_compare2(data, .group1, .group2, y, method, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group1 Grouping variables factor 1 (supports only two groups).
#' @param .group2 Grouping variables factor 1 (supports only two groups).
#' @param y Dependent variable (numeric data).
#' @param method The method of difference comparison. Such as \code{\link{TTest}},
#' \code{\link{TTest2}}, \code{\link{WilcoxTest}}, \code{\link{WilcoxTest2}},
#' \code{\link{KruskalTest}}, \code{\link{KruskalTest2}}, \code{\link{LSD}},
#' \code{\link{LSD2}}, \code{\link{HSD}}, \code{\link{HSD2}}, etc.
#' @param ... Other parameters for \code{\link[stats]{t.test}}.
#'
#' @return An \code{\link{compare2-class}} object.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta}}, \code{\link{calc_beta2}}, \code{\link{calc_compare}},
#' \code{\link{calc_alpha}}, \code{\link{calc_nemindex}}, \code{\link{calc_funguild}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_ttest <- nem |>
#'               filter_name(meta, con_crop %in% c("Y2", "Y11")) |>
#'               calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
#' nem_ttest
calc_compare2 <- function(data, .group1, .group2, y, method, ...){
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  y = deparse(substitute(y))
  result = method(data, .group1, .group2, y, ...)
  return(result)
}
