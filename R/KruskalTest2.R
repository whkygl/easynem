#' KruskalTest2
#' @description Kruskal-Wallis test was performed for grouped data.
#' @param data easynem type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @param y Dependent variable.
#' @param method2 The method for adjust p value.
#' @param ... Other parameters for KruskalTest2.
#' @return An compare2 object.
#' @export
KruskalTest2 <- function(data, .group1, .group2, y, method2 = "none", ...){
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
    rstatix::adjust_pvalue(method = method2) |>
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