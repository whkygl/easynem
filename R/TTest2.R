#' Perform t-test on easynem meta-table by treatment (two-factor)
#'
#' The \code{TTest2()} is used to perform \code{t-test} for grouped data and create
#' \code{\link{compare2-class}}. This function is only applicable to two-factor
#' analysis, see \code{\link{TTest}} for a single factor version of the function.
#'
#' Note: The \code{t-test} is only applicable to comparisons between two groups
#' of data. To facilitate code interpretation, It is recommended to use this
#' function in conjunction with the \code{\link{calc_compare2}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
#' ```
#'
#' @usage TTest2(data, .group1, .group2, y, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group1 Grouping variables factor 1 (supports only two groups).
#' @param .group2 Grouping variables factor 2 (supports only two groups).
#' @param y Dependent variable (numeric data).
#' @param ... Other parameters for \code{\link[stats]{t.test}}.
#'
#' @return An \code{\link{compare2-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest}},
#' \code{\link{WilcoxTest}}, \code{\link{WilcoxTest2}}, \code{\link{KruskalTest}},
#' \code{\link{KruskalTest2}}, \code{\link{LSD}}, \code{\link{LSD2}}, \code{\link{HSD}},
#' \code{\link{HSD2}}.
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
TTest2 <- function(data, .group1, .group2, y, ...){
  .compare2 = methods::new("compare2")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group1, .group2, y)]
  name1 = unique(meta[,2])[1]
  name2 = unique(meta[,2])[2]
  .compare2@meta = meta
    meta = meta[,-1]
    meta = meta |>
      dplyr::group_by(!!rlang::sym(.group2), !!rlang::sym(.group1)) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup()
    meta_wide <- meta |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.group1), values_from = !!rlang::sym(y), names_prefix = paste0(y,"_")) |>
      dplyr::select(-id)
    result = meta_wide |>
      dplyr::group_by(!!rlang::sym(.group2)) |>
      dplyr::do(broom::tidy(stats::t.test(as.vector(.[,2])[[1]], as.vector(.[,3])[[1]], ...)))
    result$group = paste0(name1, "-", name2)
    .compare2@result = result
  .compare2@temp = c("TTest2")
  return(.compare2)
}
