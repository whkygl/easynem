#' nem_plot
#' 
#' This is a generic function for visualization.
#' @param object easynem or other types data.
#' @param ... Other parameters.
#' @export
setGeneric("nem_plot", function(object,...){
  standardGeneric("nem_plot")
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object easynem or other types data.
#' @param level Used to adjust the size of the confidence ellipse.
#' @param type Method used to adjust the display of scatter area.
#' @param ... Other parameters for ggplot2::stat_ellipse and ggalt::geom_encircle functions.
#' @return An ggplot object.
#' @rdname beta
#' @name beta
#' @aliases nem_plot,beta-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("beta"), function(object, level = 0.6, type = 1, ...){
  my_color <- c("#4f72d2","#E64B35","#4DBBD5","#F2A900","#00A087","#3C5488",
                "#F39B7F","#0096b0","#DC0000","#c06e26",
                "#b5426a")
  meta = object@meta
  result = object@result
  temp = object@temp
  if (length(temp) == 1){
    NMDS1 = meta[,3]
    NMDS2 = meta[,4]
    group = meta[,2]
    p = ggplot2::ggplot(meta,ggplot2::aes(x=NMDS1,y=NMDS2,color=!!dplyr::sym(colnames(group))))+
      ggplot2::labs(x=colnames(NMDS1),y = colnames(NMDS2), title=temp)+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  } else {
        PCoA1 = meta[,3]
        PCoA2 = meta[,4]
        group = meta[,2]
    if (colnames(PCoA1)=="PCoA1"){
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PCoA1,y=PCoA2,color=!!dplyr::sym(colnames(group))))
    } else {
        PC1 = meta[,3]
        PC2 = meta[,4]
        group = meta[,2]
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PC1,y=PC2,color=!!dplyr::sym(colnames(group))))
    }
    p = p+ggplot2::labs(x = temp[1], y=temp[2], title=temp[3])+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  }
  p = p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed") + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object easynem or other types data.
#' @param level Used to adjust the size of the confidence ellipse.
#' @param type Method used to adjust the display of scatter area.
#' @param ... Other parameters for ggplot2::stat_ellipse and ggalt::geom_encircle functions.
#' @return An ggplot object.
#' @rdname beta2
#' @name beta2
#' @aliases nem_plot,beta2-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("beta2"), function(object, level = 0.6, type = 1, ...){
  my_color <- c("#4f72d2","#E64B35","#4DBBD5","#F2A900","#00A087","#3C5488",
                "#F39B7F","#0096b0","#DC0000","#c06e26",
                "#b5426a")
  meta = object@meta
  result = object@result
  temp = object@temp
  if (length(temp) == 1){
    NMDS1 = meta[,5]
    NMDS2 = meta[,6]
    group1 = meta[,2]
    group2 = meta[,3]
    group3 = meta[,4]
    p = ggplot2::ggplot(meta,ggplot2::aes(x=NMDS1,y=NMDS2,color=!!dplyr::sym(colnames(group1)), shape=!!dplyr::sym(colnames(group2))))+
      ggplot2::labs(x=colnames(NMDS1),y = colnames(NMDS2), title=temp)+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group3))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  } else {
        PCoA1 = meta[,5]
        PCoA2 = meta[,6]
        group1 = meta[,2]
        group2 = meta[,3]
        group3 = meta[,4]
    if (colnames(PCoA1)=="PCoA1"){
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PCoA1,y=PCoA2,color=!!dplyr::sym(colnames(group1)),shape=!!dplyr::sym(colnames(group2))))
    } else {
        PC1 = meta[,5]
        PC2 = meta[,6]
        group1 = meta[,2]
        group2 = meta[,3]
        group3 = meta[,4]
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PC1,y=PC2,color=!!dplyr::sym(colnames(group1)),shape=!!dplyr::sym(colnames(group2))))
    }
    p = p+ggplot2::labs(x = temp[1], y=temp[2], title=temp[3])+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group3))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  }
  p = p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed") + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object easynem or other types data.
#' @param type Drawing style, bar or box.
#' @param add Add mean and standard error or mean and standard deviation.
#' @param ... Other parameters for nem_plot functions.
#' @return An ggplot object.
#' @rdname compare
#' @name compare
#' @aliases nem_plot,compare-method
#' @import ggplot2
#' @import ggalt
#' @import ggpubr
#' @export
setMethod("nem_plot", signature("compare"), function(object, type = 1, add, ...){
  meta = object@meta
  temp = object@temp
  if (temp == "TTest") {
    value = meta[,3]
    group = meta[,2]
    if(type == 1){
      p = ggplot2::ggplot(meta, ggplot2::aes(x = group, y = value)) + 
        ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.5) +
        ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none") +
        ggplot2::theme_test() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(colnames(meta)[3])
      compar = list(unique(meta[,2]))
      p = p + ggpubr::stat_compare_means(comparisons = compar, method = "t.test", label = "p.signif")
    } else if (type == 2){
      compar = list(unique(meta[,2]))
      if (add == "mean_se"){
         p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean_se") +
           ggpubr::stat_compare_means(comparisons = compar, method = "t.test", label = "p.signif") + ggplot2::xlab(NULL)+
           ggplot2::theme_test() +
           ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
           ggplot2::scale_fill_discrete(guide = "none")
      } else if (add == "mean_sd"){
        p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean_sd") +
           ggpubr::stat_compare_means(comparisons = compar, method = "t.test", label = "p.signif") + ggplot2::xlab(NULL)+
           ggplot2::theme_test() +
           ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
           ggplot2::scale_fill_discrete(guide = "none")
      }
     
    } 
  } else if (temp == "WilcoxTest") {
    value = meta[,3]
    group = meta[,2]
    if(type == 1){
      p = ggplot2::ggplot(meta, ggplot2::aes(x = group, y = value)) + 
        ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.5) +
        ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none") +
        ggplot2::theme_test() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(colnames(meta)[3])
      compar = list(unique(meta[,2]))
      p = p + ggpubr::stat_compare_means(comparisons = compar, method = "wilcox.test", label = "p.signif")
    } else if (type == 2){
      compar = list(unique(meta[,2]))
      if (add == "mean_se"){
        p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean_se") +
        ggpubr::stat_compare_means(comparisons = compar, method = "wilcox.test", label = "p.signif") + ggplot2::xlab(NULL)+
        ggplot2::theme_test() +
        ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none")
      } else if (add == "mean_sd"){
        p = ggpubr::ggbarplot(meta, x = colnames(meta)[2], y = colnames(meta)[3], fill = colnames(meta)[2], width = 0.5, add = "mean_sd") +
        ggpubr::stat_compare_means(comparisons = compar, method = "wilcox.test", label = "p.signif") + ggplot2::xlab(NULL)+
        ggplot2::theme_test() +
        ggplot2::geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none")
      }
    } 
  } else if (temp == "KruskalTest") {
    value = meta[,3]
    group = meta[,2]
    wmc = object@result$wmc
    wmc = dplyr::select(dplyr::select(dplyr::mutate(wmc, names = paste(Group.1, Group.2, sep = "-")), names, everything()), -Group.1, -Group.2)
    significant = wmc[,3]
    names(significant) = wmc$names
    dif = multcompView::multcompLetters(significant)
    dif2 = as.data.frame(dif$monospacedLetters)
    colnames(dif2) <- "label"
    dif2$label <- gsub(" ", "", dif2$label)
    dif2$group <- row.names(dif2)
    if(type == 1){
      p = ggplot2::ggplot(meta, ggplot2::aes(x = group, y = value)) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.5) +
        ggplot2:: geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
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
        ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none")
      all$mean = all[,3]
      p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-er, ymax =mean+er), width = .2) +
         ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + er, label = label), vjust = -0.5)
    }
  } else if (temp == "LSD") {
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
        ggplot2:: geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
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
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 1/length(unique(meta[,2]))) +
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
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 1/length(unique(meta[,2]))) +
          ggplot2::scale_fill_discrete(guide = "none")
        meta2 = dplyr::group_by(meta, !!rlang::sym(colnames(meta)[2])) |> dplyr::summarise(max = max(.data[[colnames(meta)[3]]]))
        all = merge(meta_all, meta2, by.x = "group", by.y = colnames(meta)[2])
        all$mean = all[[colnames(meta)[3]]]
        p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-std, ymax =mean+std), width = .2) +
          ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + std, label = groups), vjust = -0.5)
      }
    }
  } else if (temp == "HSD") {
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
        ggplot2:: geom_jitter(ggplot2::aes(fill = group), shape = 21, width = 1/length(unique(meta[,2]))) +
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
        ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(colnames(meta)[2])), shape = 21, width = 1/length(unique(meta[,2]))) +
        ggplot2::scale_fill_discrete(guide = "none")
      
      all$mean = all[,3]
      p = p + ggplot2::geom_errorbar(data = all, ggplot2::aes(x = group, y = mean, ymin = mean-er, ymax =mean+er), width = .2) +
        ggplot2::geom_text(data = all, ggplot2::aes(x = group, y = mean + er, label = label), vjust = -0.5)
  }
}
p
})

#' nem_plot
#' @description For visualization of nematode community data.
#' @param object easynem or other types data.
#' @param type1 Drawing style, bar or box.
#' @param type2 Drawing style, tufted or faceted.
#' @param add Add mean and standard error or mean and standard deviation.
#' @param ... Other parameters for nem_plot functions.
#' @return An ggplot object.
#' @rdname compare2
#' @name compare2
#' @aliases nem_plot,compare2-method
#' @import ggplot2
#' @import ggalt
#' @import ggpubr
#' @importFrom stats sd
#' @export
setMethod("nem_plot", signature("compare2"), function(object, type1 = 1, type2 = 1, add, ...){
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
  p
})