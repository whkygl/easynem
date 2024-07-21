#' Perform wilcoxon-test on easynem meta-table by treatment (single factor)
#'
#' The \code{WilcoxTest()} is used to perform \code{wilcoxon-test} for grouped data and create
#' \code{\link{compare-class}}. This function is only applicable to single factor
#' analysis, see \code{\link{WilcoxTest2}} for a two factor version of the function.
#'
#' Note: The \code{wilcoxon-test} is only applicable to comparisons between two groups
#' of data. To facilitate code interpretation, It is recommended to use this
#' function in conjunction with the \code{\link{calc_compare}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare(.group = con_crop, y = pH, method = WilcoxTest)
#' ```
#'
#' @usage WilcoxTest(data, .group, y, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group Grouping variables (supports only two groups).
#' @param y Dependent variable (numeric data).
#' @param ... Other parameters for \code{\link[stats]{wilcox.test}}.
#'
#' @return An \code{\link{compare-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest2}},
#' \code{\link{TTest}}, \code{\link{WilcoxTest2}}, \code{\link{KruskalTest}},
#' \code{\link{KruskalTest2}}, \code{\link{LSD}}, \code{\link{LSD2}}, \code{\link{HSD}},
#' \code{\link{HSD2}}.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_test <- nem |>
#'               filter_name(meta, Treatments %in% c("CK", "C8")) |>
#'               calc_compare(.group = Treatments, y = Mesorhabditis, method = WilcoxTest)
#' nem_test
WilcoxTest <- function(data, .group, y, ...){
    .compare = methods::new("compare")
    meta = as.data.frame(data@meta)
    meta = meta[,c(names(meta)[1], .group, y)]
    row.names(meta) = meta[,1]
    y1 = meta[meta[,2] == unique(meta[,2])[1],3]
    y2 = meta[meta[,2] == unique(meta[,2])[2],3]
    result = stats::wilcox.test(y1, y2, ...)
    .compare@result = result
    .compare@meta = meta
    .compare@temp = c("WilcoxTest")
    return(.compare)
}
