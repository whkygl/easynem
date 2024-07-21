#' Perform Kruskal-Wallis test on easynem meta-table by treatment (two-factor)
#'
#' The \code{KruskalTest2()} is used to perform \code{Kruskal-Wallis} test for
#' grouped data and create \code{\link{compare2-class}}. This function is only
#' applicable to two-factor analysis, see \code{\link{KruskalTest}} for a
#' single factor version of the function.
#'
#' To facilitate code interpretation, It is recommended to use this function in
#' conjunction with the \code{\link{calc_compare2}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = KruskalTest)
#' ```
#'
#' @usage KruskalTest2(data, .group1, .group2, y, p.adj = "none", ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group1 Grouping variables factor 1.
#' @param .group2 Grouping variables factor 2.
#' @param y Dependent variable (numeric data).
#' @param p.adj method for correcting p-values for multiple comparisons. Default \code{p.adj = "none"}.
#' @param ... Other parameters for \code{\link[stats]{kruskal.test}}.
#'
#' @return An \code{\link{compare2-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest2}},
#' \code{\link{TTest}}, \code{\link{WilcoxTest2}}, \code{\link{WilcoxTest}},
#' \code{\link{KruskalTest}}, \code{\link{LSD}}, \code{\link{LSD2}}, \code{\link{HSD}},
#' \code{\link{HSD2}}.
#'
#' @references
#' R in Action: Data Analysis and Graphics with R, Second Edition by Robert I. Kabacoff,
#' published by Manning Publications. 178 South Hill Drive, Westampton, NJ 08060 USA.
#' Copyright 2015 by Manning Publications.
#'
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_test <- nem |>
#'               calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = KruskalTest2)
#' nem_test
KruskalTest2 <- function(data, .group1, .group2, y, p.adj = "none", ...){
  .compare2 = methods::new("compare2")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group1, .group2, y)]
  .compare2@meta = meta
  .compare2@temp = c("KruskalTest2")
  meta = meta[,-1]
  meta = meta |>
      dplyr::group_by(!!rlang::sym(.group2), !!rlang::sym(.group1)) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup()
  meta_wide <- meta |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.group2), values_from = !!rlang::sym(y), names_prefix = paste0(y,"_")) |>
      dplyr::select(-id)
  results <- lapply(names(meta_wide)[2:ncol(meta_wide)], function(col) {
  stats::kruskal.test(meta_wide[[col]] ~ meta_wide[[.group1]], ...)
})
  names(results) <- names(meta_wide)[2:ncol(meta_wide)]
  meta_long <- meta[,-4]
  formu <- stats::as.formula(paste0(names(meta_long)[3], " ~ ", names(meta_long)[1]))
  results2 <- meta_long |>
    dplyr::group_by(!!rlang::sym(names(meta_long)[2])) |>
    rstatix::wilcox_test(formu, ...) |>
    rstatix::adjust_pvalue(method = p.adj) |>
    rstatix::add_significance("p")
results2$p.adj.signif <- ifelse(results2$p.adj > 0.1, "ns",
                         ifelse(results2$p.adj > 0.05 & results2$p.adj <= 0.1, ".",
                         ifelse(results2$p.adj > 0.01 & results2$p.adj <= 0.05, "*",
                         ifelse(results2$p.adj > 0.001 & results2$p.adj <= 0.01, "**",
                         ifelse(results2$p.adj > 0.0001 & results2$p.adj <= 0.001, "***",
                         ifelse(results2$p.adj <= 0.0001, "****", NA))))))
results$WilcoxTest <- results2
.compare2@result = results
  return(.compare2)
}
