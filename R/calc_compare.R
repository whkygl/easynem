#' Multiple comparisons between treatments (single factor)
#'
#' The \code{calc_compare} is used for multiple comparisons between different
#' treatments and create \code{\link{compare-class}}. This function is only
#' applicable to single factor analysis, see \code{\link{calc_compare2}} for a
#' two factor version of the function.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_compare <- nem |> calc_compare(.group = con_crop, y = pH, method = TTest)
#' ```
#'
#' @usage calc_compare(data, .group, y, method, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group Grouping variables (supports only two groups).
#' @param y Dependent variable (numeric data).
#' @param method The method of difference comparison. Such as \code{\link{TTest}},
#' \code{\link{TTest2}}, \code{\link{WilcoxTest}}, \code{\link{WilcoxTest2}},
#' \code{\link{KruskalTest}}, \code{\link{KruskalTest2}}, \code{\link{LSD}},
#' \code{\link{LSD2}}, \code{\link{HSD}}, \code{\link{HSD2}}, etc.
#' @param ... Other parameters for \code{\link[stats]{t.test}}.
#'
#' @return An \code{\link{compare-class}} object.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta}}, \code{\link{calc_beta2}}, \code{\link{calc_compare2}},
#' \code{\link{calc_alpha}}, \code{\link{calc_nemindex}}, \code{\link{calc_funguild}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_ttest <- nem |>
#'               filter_name(meta, Treatments %in% c("CK", "C8")) |>
#'               calc_compare(.group = Treatments, y = Mesorhabditis, method = TTest)
#' nem_ttest
calc_compare <- function(data, .group, y, method, ...){
    .group = deparse(substitute(.group))
    y = deparse(substitute(y))
    result = method(data, .group, y, ...)
    return(result)
}
