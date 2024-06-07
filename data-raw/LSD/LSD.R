library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"), meta = easynem_example("meta.csv"))
LSD <- function(data, .group, y, ...){
  .compare = methods::new("compare")
  meta = as.data.frame(data@meta)
  meta = meta[,c(1, which(names(meta) %in% c(.group, y)))]
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
hehe <- calc_compare(bac, con_crop, pH, method = LSD)
nem_plot <- function(object, type = 1, add, ...){
  # object = hehe
  meta = object@meta
  temp = object@temp
  if (temp == "LSD") {
    value = meta[,3]
    group = meta[,2]
    meta1 = object@result$LSD$means[,-1]
    meta1$group = rownames(meta1)
    meta2 = object@result$LSD$groups
    meta2$group = rownames(meta2)
    meta_all = merge(meta1,meta2,by = "group")
    if (type == 1){
      p = ggplot2::ggplot(meta, ggplot2::aes(x = group, y = value)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.5) +
        ggplot2:: geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 0.1) +
        ggplot2::scale_fill_discrete(guide = "none") +
        ggplot2::theme_test() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(colnames(meta)[3])
      meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(max = max(.data[[colnames(meta)[3]]]))
      all = merge(meta_all, meta2, by.x = "group", by.y = colnames(meta)[2])
      all$mean = all[[colnames(meta)[3]]]
      p = p + ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = max, label = groups), vjust = -0.5)
    } else if (type == 2){
      if (add == "mean_se"){
        p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean") +
          ggplot2::xlab(NULL)+
          ggplot2::theme_test() +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 0.1) +
          ggplot2::scale_fill_discrete(guide = "none")
        meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(max = max(.data[[colnames(meta)[3]]]))
        all = merge(meta_all, meta2, by.x = "group", by.y = colnames(meta)[2])
        all$mean = all[[colnames(meta)[3]]]
        p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-se, ymax =mean+se), width = .2) +
          ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + se, label = groups), vjust = -0.5)
      } else if (add == "mean_sd") {
        p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean") +
          ggplot2::xlab(NULL)+
          ggplot2::theme_test() +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 0.1) +
          ggplot2::scale_fill_discrete(guide = "none")
        meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(max = max(.data[[colnames(meta)[3]]]))
        all = merge(meta_all, meta2, by.x = "group", by.y = colnames(meta)[2])
        all$mean = all[[colnames(meta)[3]]]
        p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-std, ymax =mean+std), width = .2) +
          ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + std, label = groups), vjust = -0.5)
      }
    }
  }
  return(p)
}
hehe <- bac |> calc_compare(con_crop, pH, method = LSD) |> nem_plot()
hehe
hehe <- bac |> calc_compare(con_crop, pH, method = LSD) |> nem_plot(type = 2, add = "mean_se")
hehe
