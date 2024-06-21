# devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), 
                tax = easynem_example("bactax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- bac |> filter_name(meta, con_crop %in% c("Y5","Y8")) |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = WilcoxTest2)
hehe
hehe <- bac |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = KruskalTest2)
hehe
hehe <- bac  |> calc_compare2(.group1 = season, .group2 = con_crop, y = pH, method = KruskalTest2)
hehe
setMethod("nem_plot", signature("compare2"), function(object, type1 = 1, type2 = 1, add, ...){
  # object = hehe
  meta = object@meta
  meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2]), !!rlang::sym(colnames(meta)[3])) |> 
    dplyr::summarise(max = max(.data[[colnames(meta)[4]]]),
                      mean = mean(!!rlang::sym(colnames(meta)[4])), 
                      se = stats::sd(!!rlang::sym(colnames(meta)[4]))/dplyr::n(),
                      sd = stats::sd(!!rlang::sym(colnames(meta)[4])))
  temp = object@temp
  result = object@result
  if(temp == "TTest2"){
    for (i in 1:nrow(result)){
      # i = 2
      sig = result[i,c(1,6,12)]
      significant = as.data.frame(sig)[,2]
      names(significant) = sig$group
      dif = multcompView::multcompLetters(significant)
      dif2 = as.data.frame(dif$Letters)
      colnames(dif2) <- "label"
      dif2$label <- gsub(" ", "", dif2$label)
      dif2[[colnames(meta)[2]]] = row.names(dif2)
      dif2[[colnames(meta)[3]]] = sig[[colnames(meta)[3]]]
      meta2 = dplyr::left_join(meta2, dif2, by = c(colnames(meta)[2], colnames(meta)[3]))
    }
    label_cols <- grep("^label\\.", colnames(meta2), value = TRUE)
    meta2$label <- apply(meta2[label_cols], 1, function(row) {
      paste(stats::na.omit(row), collapse = "")
    })
    meta2 <- meta2[, !colnames(meta2) %in% label_cols]
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                               y = !!rlang::sym(names(meta)[4]), 
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),position = ggplot2::position_dodge(0.8)) + 
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21, position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::geom_text(data = meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), 
                                                        y = !!rlang::sym(names(meta2)[3]), 
                                                        label = !!rlang::sym(names(meta2)[7]),
                                                        vjust = -0.5), 
                             position = ggplot2::position_dodge(0.8)) +
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
        
      } else if (type2 == 2){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[2]))) +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[2])), shape = 21, width = 1/nrow(meta2)/2) +
          ggplot2::scale_fill_discrete(guide = "none")+
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
        compar = list(unique(meta[,2]))
        formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
        p = p + ggpubr::stat_compare_means(comparisons = compar, method = "t.test", label = "p.signif", ...) + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
      }
    } else if (type1 == 2){
      if (type2 == 1){
        p = ggplot2::ggplot(meta2, ggplot2:: aes(x = !!rlang::sym(names(meta2)[1]), 
                                                y = !!rlang::sym(names(meta2)[4]), 
                                                fill = !!rlang::sym(names(meta2)[2]))) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(0.8), color = "black", width = 0.75) +
          ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                                         y = !!rlang::sym(names(meta)[4]) , 
                                                         fill = !!rlang::sym(names(meta)[3])), 
                                                         shape = 21, 
                               position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::ylab(names(meta)[4]) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_test()
        if (add == "mean_se"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        } else if (add == "mean_sd"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean+sd), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + sd,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        }
          
      } else if (type2 == 2){
        formu = stats::as.formula(paste0(names(meta)[4], "~", names(meta)[2]))
        stat.test = ggpubr::compare_means(formu, data = meta, group.by = names(meta)[3], method = "t.test", ...)
        meta3 = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2])) |> dplyr::summarise(p.position = max(max))
        stat.test = merge(stat.test, meta3, by = names(meta2)[2])
        if (add == "mean_se"){
          p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), y = !!rlang::sym(names(meta2)[4]), fill = names(meta[2]))) +
            ggplot2::geom_bar(stat = "identity",ggplot2::aes(fill = !!rlang::sym(names(meta2)[2])), color = "black",width = 0.75) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = 0.75/4)+
            ggplot2::theme_test() +
            ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),y = !!rlang::sym(names(meta)[4]), fill = !!rlang::sym(names(meta)[3])), shape = 21, width = 0.75/4) +
            ggplot2::scale_fill_discrete(guide = "none") +
            ggpubr::stat_pvalue_manual(
              stat.test, label = "p.signif", y.position = "p.position"
            ) +
            ggplot2::xlab(NULL)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        } else if (add == "mean_sd"){
          p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), y = !!rlang::sym(names(meta2)[4]), fill = names(meta[2]))) +
            ggplot2::geom_bar(stat = "identity",ggplot2::aes(fill = !!rlang::sym(names(meta2)[2])), color = "black",width = 0.75) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-sd, ymax = mean+sd), width = 0.75/4)+
            ggplot2::theme_test() +
            ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),y = !!rlang::sym(names(meta)[4]), fill = !!rlang::sym(names(meta)[3])), shape = 21, width = 0.75/4) +
            ggplot2::scale_fill_discrete(guide = "none") +
            ggpubr::stat_pvalue_manual(
              stat.test, label = "p.signif", y.position = "p.position"
            ) +
            ggplot2::xlab(NULL)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        }
      }
    }
  } else if (temp == "WilcoxTest2"){
    for (i in 1:nrow(result)){
      # i = 2
      sig = result[i,c(1,3,6)]
      significant = as.data.frame(sig)[,2]
      names(significant) = sig$group
      dif = multcompView::multcompLetters(significant)
      dif2 = as.data.frame(dif$Letters)
      colnames(dif2) <- "label"
      dif2$label <- gsub(" ", "", dif2$label)
      dif2[[colnames(meta)[2]]] = row.names(dif2)
      dif2[[colnames(meta)[3]]] = sig[[colnames(meta)[3]]]
      meta2 = dplyr::left_join(meta2, dif2, by = c(colnames(meta)[2], colnames(meta)[3]))
    }
    label_cols <- grep("^label\\.", colnames(meta2), value = TRUE)
    meta2$label <- apply(meta2[label_cols], 1, function(row) {
      paste(stats::na.omit(row), collapse = "")
    })
    meta2 <- meta2[, !colnames(meta2) %in% label_cols]
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                               y = !!rlang::sym(names(meta)[4]), 
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),position = ggplot2::position_dodge(0.8)) + 
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21, position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::geom_text(data = meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), 
                                                        y = !!rlang::sym(names(meta2)[3]), 
                                                        label = !!rlang::sym(names(meta2)[7]),
                                                        vjust = -0.5), 
                             position = ggplot2::position_dodge(0.8)) +
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
      
        } else if (type2 == 2){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[2]))) +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[2])), shape = 21, width = 1/nrow(meta2)/2) +
          ggplot2::scale_fill_discrete(guide = "none")+
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
        compar = list(unique(meta[,2]))
        formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
        p = p + ggpubr::stat_compare_means(comparisons = compar, method = "wilcox.test", label = "p.signif", ...) + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
      }
    } else if (type1 == 2){
      if (type2 == 1){
          p = ggplot2::ggplot(meta2, ggplot2:: aes(x = !!rlang::sym(names(meta2)[1]), 
                                                   y = !!rlang::sym(names(meta2)[4]), 
                                                   fill = !!rlang::sym(names(meta2)[2]))) +
            ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(0.8), color = "black", width = 0.75) +
            ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                                           y = !!rlang::sym(names(meta)[4]) , 
                                                           fill = !!rlang::sym(names(meta)[3])), 
                                 shape = 21, 
                                 position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
            ggplot2::ylab(names(meta)[4]) +
            ggplot2::xlab(NULL) +
            ggplot2::theme_test()
          if (add == "mean_se"){
            p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), position = ggplot2::position_dodge(.8), width = 0.75/4) +
              ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
          } else if (add == "mean_sd"){
            p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean+sd), position = ggplot2::position_dodge(.8), width = 0.75/4) +
              ggplot2::geom_text(ggplot2::aes(y = mean + sd,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
          }
      
          } else if (type2 == 2){
        formu = stats::as.formula(paste0(names(meta)[4], "~", names(meta)[2]))
        stat.test = ggpubr::compare_means(formu, data = meta, group.by = names(meta)[3], method = "wilcox.test", ...)
        meta3 = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2])) |> dplyr::summarise(p.position = max(max))
        stat.test = merge(stat.test, meta3, by = names(meta2)[2])
        if (add == "mean_se"){
          p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), y = !!rlang::sym(names(meta2)[4]), fill = names(meta[2]))) +
            ggplot2::geom_bar(stat = "identity",ggplot2::aes(fill = !!rlang::sym(names(meta2)[2])), color = "black",width = 0.75) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = 0.75/4)+
            ggplot2::theme_test() +
            ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),y = !!rlang::sym(names(meta)[4]), fill = !!rlang::sym(names(meta)[3])), shape = 21, width = 0.75/4) +
            ggplot2::scale_fill_discrete(guide = "none") +
            ggpubr::stat_pvalue_manual(
              stat.test, label = "p.signif", y.position = "p.position"
            ) +
            ggplot2::xlab(NULL)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        
          } else if (add == "mean_sd"){
          p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), y = !!rlang::sym(names(meta2)[4]), fill = names(meta[2]))) +
            ggplot2::geom_bar(stat = "identity",ggplot2::aes(fill = !!rlang::sym(names(meta2)[2])), color = "black",width = 0.75) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-sd, ymax = mean+sd), width = 0.75/4)+
            ggplot2::theme_test() +
            ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),y = !!rlang::sym(names(meta)[4]), fill = !!rlang::sym(names(meta)[3])), shape = 21, width = 0.75/4) +
            ggplot2::scale_fill_discrete(guide = "none") +
            ggpubr::stat_pvalue_manual(
              stat.test, label = "p.signif", y.position = "p.position"
            ) +
            ggplot2::xlab(NULL)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        }
      }
    }
  } else if (temp == "KruskalTest2"){
    result = result$WilcoxTest
    result$group <- paste(result$group1, result$group2, sep = "-")
    result_grouped <- result |> dplyr::group_by(!!rlang::sym(names(meta)[3])) %>%
      dplyr::group_split()
    results <- lapply(result_grouped, function(group) {
      sig <- group[, c(1, 9, 12)]
      significant <- as.data.frame(sig)[, 2]
      names(significant) <- sig$group
      dif <- multcompView::multcompLetters(significant)
      dif2 <- as.data.frame(dif$Letters)
      colnames(dif2) <- "label"
      dif2$label <- gsub(" ", "", dif2$label)
      dif2[[colnames(meta)[2]]] <- row.names(dif2)
      dif2[[colnames(meta)[3]]] <- unique(sig[[colnames(meta)[3]]])
      dif2
    })
    results_combined <- do.call(rbind, results)
    meta2 <- dplyr::left_join(meta2, results_combined, by = c(colnames(meta)[2], colnames(meta)[3]))
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                               y = !!rlang::sym(names(meta)[4]), 
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),position = ggplot2::position_dodge(0.8)) + 
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21, position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::geom_text(data = meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), 
                                                        y = !!rlang::sym(names(meta2)[3]), 
                                                        label = !!rlang::sym(names(meta2)[7]),
                                                        vjust = -0.5), 
                             position = ggplot2::position_dodge(0.8)) +
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
      } else if (type2 == 2){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), shape = 21, width = 1/nrow(meta2)/2) +
          ggplot2::scale_fill_discrete(guide = "none")+
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
        compar = list(unique(meta[,2]))
        formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
        p = p + ggplot2::geom_text(data = meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), 
                                                              y = !!rlang::sym(names(meta2)[3]), 
                                                              label = !!rlang::sym(names(meta2)[7]),
                                                              vjust = -0.5)) + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
      }
    } else if (type1 == 2){
      if (type2 == 1){
        p = ggplot2::ggplot(meta2, ggplot2:: aes(x = !!rlang::sym(names(meta2)[1]), 
                                                 y = !!rlang::sym(names(meta2)[4]), 
                                                 fill = !!rlang::sym(names(meta2)[2]))) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(0.8), color = "black", width = 0.75) +
          ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                                         y = !!rlang::sym(names(meta)[4]) , 
                                                         fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21, 
                               position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::ylab(names(meta)[4]) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_test()
        if (add == "mean_se"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        } else if (add == "mean_sd"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean+sd), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + sd,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        }
        
      } else if (type2 == 2){
        p = ggplot2::ggplot(meta2, ggplot2:: aes(x = !!rlang::sym(names(meta2)[1]), 
                                                 y = !!rlang::sym(names(meta2)[4]), 
                                                 fill = !!rlang::sym(names(meta2)[2]))) +
          ggplot2::geom_bar(stat = "identity",color = "black", width = 0.75) +
          ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                                         y = !!rlang::sym(names(meta)[4]) , 
                                                         fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21)+
          ggplot2::ylab(names(meta)[4]) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_test()
        if (add == "mean_se"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
          
        } else if (add == "mean_sd"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean+sd), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + sd,label = label),vjust = -0.5)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        }
      }
    }
  } else if (temp == "LSD2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "HSD2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } 
})
p = hehe |> nem_plot()
p
p = bac |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = KruskalTest2) |> nem_plot(type1 = 2, type2 = 2, add = "mean_sd")
p
