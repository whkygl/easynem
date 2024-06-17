devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), 
                tax = easynem_example("bactax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- calc_compare2(bac, season, con_crop, pH, method = WilcoxTest2)
hehe
KruskalTest2 <- function(data, .group1, .group2, y, method2 = "none", ...){
#   data = bac
#   .group1 = "con_crop"
#   .group2 = "season"
#   y = "pH"
  .compare2 = methods::new("compare2")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group1, .group2, y)]
  .compare2@meta = meta
#   name1 = unique(meta[,2])[1]
#   name2 = unique(meta[,2])[2]
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
  kruskal.test(meta_wide[[col]] ~ meta_wide[[.group1]])
})
  names(results) <- names(meta_wide)[2:ncol(meta_wide)]
  meta_long <- meta[,-4]
  formu <- as.formula(paste0(names(meta_long)[3], " ~ ", names(meta_long)[1]))
  results2 <- meta_long |>
    dplyr::group_by(!!rlang::sym(names(meta_long)[2])) |> 
    rstatix::wilcox_test(formu, ...) |>
    rstatix::adjust_pvalue(method = method2) |>
    rstatix::add_significance("p")
  # 假设results2是你的数据框，并且p.adj是需要检查的列
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
hehe <- calc_compare2(bac, season, con_crop, pH, method = KruskalTest2, method2 = "bonferroni", paired = TRUE)
hehe
hehe <- calc_compare2(bac, con_crop, season, pH, method = KruskalTest2)
hehe
