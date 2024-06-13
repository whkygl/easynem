library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"), meta = easynem_example("meta.csv"))
HSD <- function(data, .group, y, ...){
  # data = bac
  # .group = "con_crop"
  # y = "pH"
  .compare = methods::new("compare")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group, y)]
  row.names(meta) = meta[,1]
  formula_str <- paste(y, "~", .group)
  formula <- stats::as.formula(formula_str)
  fit = stats::aov(formula, data = meta, ...)
  .compare@meta = meta
  .compare@result$`Fit an Analysis of Variance Model` = summary(fit)
  .compare@temp = c("HSD")
  Tukey_HSD = stats::TukeyHSD(fit, ...)
  .compare@result$HSD = Tukey_HSD
  return(.compare)
}
hehe <- calc_compare(bac, con_crop, pH, method = HSD)
nem_plot <- function(object, type = 1, add, ...) {
  # object = hehe
  meta = object@meta
  temp = object@temp
  if (temp == "HSD") {
    value = meta[,3]
    group = meta[,2]
    wmc = as.data.frame(object@result$HSD[[colnames(object@meta)[2]]])
    significant = wmc[,4]
    names(significant) = rownames(wmc)
    dif = multcompView::multcompLetters(significant)
    dif2 = as.data.frame(dif$monospacedLetters)
    colnames(dif2) <- "label"
    dif2$group <- rownames(dif2)
    if(type == 1){
      p = ggplot2::ggplot(meta, ggplot2::aes(x = group, y = value)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.5) +
        ggplot2:: geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 0.1) +
        ggplot2::scale_fill_discrete(guide = "none") +
        ggplot2::theme_test() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(colnames(meta)[3])
      meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(max = max(.data[[colnames(meta)[3]]]))
      all = merge(dif2, meta2, by.x = "group", by.y = colnames(meta)[2])
      p = p + ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = max, label = label), vjust = -0.5)
    } else if (type == 2){
      if (add == "mean_se") {
        meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(mean = mean(!!rlang::sym(colnames(meta)[3])), er = stats::sd(!!rlang::sym(colnames(meta)[3]))/dplyr::n())
      } else if (add == "mean_sd") {
        meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(mean = mean(!!rlang::sym(colnames(meta)[3])), er = stats::sd(!!rlang::sym(colnames(meta)[3])))
      }
      all = merge(dif2, meta2, by.x = "group", by.y = colnames(meta)[2])
      colnames(all)[3] = colnames(meta)[3]
      p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean") +
        ggplot2::xlab(NULL)+
        ggplot2::theme_test() +
        ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 0.1) +
        ggplot2::scale_fill_discrete(guide = "none")
      
      all$mean = all[,3]
      p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-er, ymax =mean+er), width = .2) +
        ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + er, label = label), vjust = -0.5)
  }
  }
}
hehe <- bac |> calc_compare(con_crop, pH, method = HSD) |> nem_plot()
hehe
hehe <- bac |> calc_compare(con_crop, pH, method = HSD) |> nem_plot(type = 2, add = "mean_se")
hehe
hehe <- bac |> calc_compare(con_crop, pH, method = HSD) |> nem_plot(type = 2, add = "mean_sd")
hehe
