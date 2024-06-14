# devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), 
                tax = easynem_example("bactax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- bac |> filter_name(meta, con_crop %in% c("Y2","Y11")) |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
hehe
hehe <- bac  |> calc_compare2(.group1 = season, .group2 = con_crop, y = pH, method = TTest2)
hehe
setMethod("nem_plot", signature("compare2"), function(object, type1 = 1, type2 = 1, add, ...){
  # object = hehe
  meta = object@meta
  meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2]), !!rlang::sym(colnames(meta)[3])) |> dplyr::summarise(max = max(.data[[colnames(meta)[4]]]),
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
      dif2 = as.data.frame(dif$monospacedLetters)
      colnames(dif2) <- "label"
      dif2$label <- gsub(" ", "", dif2$label)
      dif2[[colnames(meta)[2]]] = row.names(dif2)
      dif2[[colnames(meta)[3]]] = sig[[colnames(meta)[3]]]
      meta2 = dplyr::left_join(meta2, dif2, by = c(colnames(meta)[2], colnames(meta)[3]))
    }
    label_cols <- grep("^label\\.", colnames(meta2), value = TRUE)
    meta2$label <- apply(meta2[label_cols], 1, function(row) {
      paste(na.omit(row), collapse = "")
    })
    meta2 <- meta2[, !colnames(meta2) %in% label_cols]
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), 
                                               y = !!rlang::sym(names(meta)[4]), 
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3]))) + 
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), 
                               shape = 21, position = ggplot2::position_jitterdodge())+
          ggplot2::geom_text(data = meta2, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]), 
                                                        y = !!rlang::sym(names(meta2)[3]), 
                                                        label = !!rlang::sym(names(meta2)[7]),
                                                        vjust = -0.5), 
                             position = ggplot2::position_jitterdodge()) +
          ggplot2::theme_test() +
          ggplot2::xlab(NULL)
        
      } else if (type2 == 2){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]),
                                               fill = !!rlang::sym(names(meta)[2])))
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "WilcoxTest2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "KruskalTest2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
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
TTest2 <- function(data, .group1, .group2, y, ...){
  data = bac
  .group1 = "season"
  .group2 = "con_crop"
  y = "pH"
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
