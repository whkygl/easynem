################################################################################
#' Visualize the results of the calculation
#'
#' The \code{nem_plot()} is used to visualize the calculation results and is a
#' generalized function for multiple classes including \code{\link{beta-class}},
#' \code{\link{beta2-class}}, \code{\link{compare-class}}, \code{\link{compare2-class}},
#' \code{\link{ef-class}}, \code{\link{ef2-class}}, \code{\link{funguild-class}},
#' \code{\link{funguild2-class}}, \code{\link{mf-class}}, \code{\link{mf2-class}},
#' \code{\link{ter-class}}, \code{\link{ter2-class}}, etc.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_plot <- nem |> calc_beta(pca, Treatments, method = "bray") |> nem_plot()
#' ```
#'
#' @param object \code{\link{beta-class}} or other types data (\code{\link{beta2-class}},
#' \code{\link{compare-class}}, \code{\link{compare2-class}}, \code{\link{ef-class}},
#' \code{\link{ef2-class}}, \code{\link{funguild-class}}, \code{\link{funguild2-class}},
#' \code{\link{mf-class}}, \code{\link{mf2-class}}, \code{\link{ter-class}},
#' \code{\link{ter2-class}}, etc.).
#' @param ... Other parameters to be expanded.
#'
#' @export
setGeneric("nem_plot", function(object, ...){
  standardGeneric("nem_plot")
})
################################################################################
#' Visualization of beta diversity results (single factor)
#'
#' The \code{\link{nem_plot}} function is generalized to the \code{\link{beta-class}}
#' and is used to visualize the single-factor beta diversity results.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_plot <- nem |> calc_beta(pca, Treatments, method = "bray") |> nem_plot()
#' ```
#'
#' @param object A \code{\link{beta-class}} object.
#' @param level Used to adjust the size of the confidence ellipse. Default
#' \code{level = 0.6}. See \code{\link[ggplot2]{stat_ellipse}}.
#' @param type Method used to adjust the display of scatter area. \code{type = 1},
#' displays as a confidence ellipse; \code{type = 2}, displays as a polygon.
#' Default \code{type = 1}.
#' @param ... Other parameters to be expanded.
#'
#' @return An \code{gg} or \code{ggplot} object.
#'
#' @seealso
#' The \code{nem_plot()} is used to visualize the calculation results and is a
#' generalized function for multiple classes including \code{\link{beta-class}},
#' \code{\link{beta2-class}}, \code{\link{compare-class}}, \code{\link{compare2-class}},
#' \code{\link{ef-class}}, \code{\link{ef2-class}}, \code{\link{funguild-class}},
#' \code{\link{funguild2-class}}, \code{\link{mf-class}}, \code{\link{mf2-class}},
#' \code{\link{ter-class}}, \code{\link{ter2-class}}, etc.
#'
#' @rdname nem_plot-methods
#' @aliases nem_plot,beta-method
#' @import ggplot2
#' @import ggalt
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_plot <- nem |>
#'             calc_beta(pcoa, Treatments, method = "bray") |>
#'             nem_plot(level = 0)
#' nem_plot
#' nem_plot <- nem |>
#'             calc_beta(nmds, Treatments, method = "bray") |>
#'             nem_plot(type = 2)
#' nem_plot
setMethod("nem_plot", signature("beta"), function(object, level = 0.6, type = 1, ...){
  meta = object@meta
  result = object@result
  temp = object@temp
  if (length(temp) == 1){
    NMDS1 = meta[,3]
    NMDS2 = meta[,4]
    group = meta[,2]
    p = ggplot2::ggplot(meta,ggplot2::aes(x=NMDS1,y=NMDS2,color=!!dplyr::sym(colnames(group))))+
      ggplot2::labs(x=colnames(NMDS1),y = colnames(NMDS2), title=temp)+ggplot2::geom_point(size=4)+
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
################################################################################
#' Visualization of beta diversity results (two-factor)
#'
#' The \code{\link{nem_plot}} function is generalized to the \code{\link{beta2-class}}
#' and is used to visualize the two-factor beta diversity results.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_plot <- nem |> calc_beta2(pca, con_crop, season, method = "bray") |> nem_plot()
#' ```
#'
#' @param object A \code{\link{beta2-class}} object.
#' @param level Used to adjust the size of the confidence ellipse. Default
#' \code{level = 0.6}. See \code{\link[ggplot2]{stat_ellipse}}.
#' @param type Method used to adjust the display of scatter area. \code{type = 1},
#' displays as a confidence ellipse; \code{type = 2}, displays as a polygon.
#' Default \code{type = 1}.
#' @param ... Other parameters to be expanded.
#'
#' @return An \code{gg} or \code{ggplot} object.
#'
#' @seealso
#' The \code{nem_plot()} is used to visualize the calculation results and is a
#' generalized function for multiple classes including \code{\link{beta-class}},
#' \code{\link{beta2-class}}, \code{\link{compare-class}}, \code{\link{compare2-class}},
#' \code{\link{ef-class}}, \code{\link{ef2-class}}, \code{\link{funguild-class}},
#' \code{\link{funguild2-class}}, \code{\link{mf-class}}, \code{\link{mf2-class}},
#' \code{\link{ter-class}}, \code{\link{ter2-class}}, etc.
#'
#' @rdname nem_plot-methods
#' @aliases nem_plot,beta2-method
#' @import ggplot2
#' @import ggalt
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_plot <- nem |>
#'             calc_beta2(pcoa, con_crop, season, method = "bray") |>
#'             nem_plot(level = 0)
#' nem_plot
#' nem_plot <- nem |>
#'             calc_beta2(nmds, con_crop, season, method = "bray") |>
#'             nem_plot(type = 2)
#' nem_plot
setMethod("nem_plot", signature("beta2"), function(object, level = 0.6, type = 1, ...){
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
################################################################################
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
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]),
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),position = ggplot2::position_dodge(0.8)) +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),
                               shape = 21, position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::geom_text(data = result, ggplot2::aes(x = !!rlang::sym(names(result)[2]),
                                                        y = !!rlang::sym("Max"),
                                                        label = !!rlang::sym("label"),
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
        p = p + ggplot2::geom_text(data = result, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]),
                                                              y = !!rlang::sym("Max"),
                                                              label = !!rlang::sym("label"),
                                                              vjust = -0.5)) + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
      }
    } else if (type1 == 2){
      if (type2 == 1){
        names(result)[13] = "mean"
        p = ggplot2::ggplot(result, ggplot2:: aes(x = !!rlang::sym(names(result)[2]),
                                                 y = !!rlang::sym("mean"),
                                                 fill = !!rlang::sym(names(result)[1]))) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(0.8), color = "black", width = 0.75) +
          ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                                         y = !!rlang::sym(names(meta)[4]) ,
                                                         fill = !!rlang::sym(names(meta)[3])),
                               shape = 21,
                               position = ggplot2::position_jitterdodge(0.8/nrow(result)))+
          ggplot2::ylab(names(meta)[4]) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_test()
        if (add == "mean_se"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        } else if (add == "mean_sd"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std, ymax = mean+std), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + std,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        }
      } else if (type2 == 2){
        names(result)[13] = "mean"
        p = ggplot2::ggplot(result, ggplot2:: aes(x = !!rlang::sym(names(result)[2]),
                                                  y = !!rlang::sym(names(result)[13]),
                                                  fill = !!rlang::sym(names(result)[1]))) +
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
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std, ymax = mean+std), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + std,label = label),vjust = -0.5)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        }
      }
    }
  } else if (temp == "HSD2"){
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                               y = !!rlang::sym(names(meta)[4]),
                                               fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),position = ggplot2::position_dodge(0.8)) +
          ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])),
                               shape = 21, position = ggplot2::position_jitterdodge(0.8/nrow(meta2)))+
          ggplot2::geom_text(data = result, ggplot2::aes(x = !!rlang::sym(names(result)[2]),
                                                         y = !!rlang::sym("Max"),
                                                         label = !!rlang::sym("label"),
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
        p = p + ggplot2::geom_text(data = result, ggplot2::aes(x = !!rlang::sym(names(meta2)[1]),
                                                               y = !!rlang::sym("Max"),
                                                               label = !!rlang::sym("label"),
                                                               vjust = -0.5)) + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
      }
    } else if (type1 == 2){
      if (type2 == 1){
        names(result)[11] = "mean"
        p = ggplot2::ggplot(result, ggplot2:: aes(x = !!rlang::sym(names(result)[2]),
                                                  y = !!rlang::sym("mean"),
                                                  fill = !!rlang::sym(names(result)[1]))) +
          ggplot2::geom_bar(stat = "identity",position = ggplot2::position_dodge(0.8), color = "black", width = 0.75) +
          ggplot2::geom_jitter(data = meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]),
                                                         y = !!rlang::sym(names(meta)[4]) ,
                                                         fill = !!rlang::sym(names(meta)[3])),
                               shape = 21,
                               position = ggplot2::position_jitterdodge(0.8/nrow(result)))+
          ggplot2::ylab(names(meta)[4]) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_test()
        if (add == "mean_se"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean+se), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + se,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        } else if (add == "mean_sd"){
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std, ymax = mean+std), position = ggplot2::position_dodge(.8), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + std,label = label),vjust = -0.5,position = ggplot2::position_dodge(0.8))
        }
      } else if (type2 == 2){
        names(result)[11] = "mean"
        p = ggplot2::ggplot(result, ggplot2:: aes(x = !!rlang::sym(names(result)[2]),
                                                  y = !!rlang::sym(names(result)[11]),
                                                  fill = !!rlang::sym(names(result)[1]))) +
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
          p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std, ymax = mean+std), width = 0.75/4) +
            ggplot2::geom_text(ggplot2::aes(y = mean + std,label = label),vjust = -0.5)
          formu = stats::as.formula(paste0(".", "~", names(meta)[3]))
          p = p + ggplot2::facet_wrap(formu, scales = "free_y", nrow = 1)
        }
      }
    }
  }
  p
})

#' nem_plot
#' @description For visualization of nematode community data.
#' @param object funguild or other types data.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname funguild
#' @name funguild
#' @aliases nem_plot,funguild-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("funguild"), function(object, ...){
  # object = hehe2
  meta = object@result
  meta2 = stats::na.omit(meta)
  p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym("SI"), y = !!rlang::sym("EI"), color = !!rlang::sym(names(meta2)[2]))) +
    ggplot2::geom_hline(yintercept = 50) +
    ggplot2::geom_vline(xintercept = 50) +
    ggplot2::geom_point() + ggplot2::theme_test() +
    ggplot2::ylab("Enrichment Index") +
    ggplot2::xlab("Structure Index")
  p
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object funguild2 or other types data.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname funguild2
#' @name funguild2
#' @aliases nem_plot,funguild2-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("funguild2"), function(object, ...){
  # object = hehe2
  meta = object@result
  meta2 = stats::na.omit(meta)
  p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym("SI"), y = !!rlang::sym("EI"), color = !!rlang::sym(names(meta2)[2]), shape = !!rlang::sym(names(meta2)[3]))) +
    ggplot2::geom_hline(yintercept = 50) +
    ggplot2::geom_vline(xintercept = 50) +
    ggplot2::geom_point() + ggplot2::theme_test() +
    ggplot2::ylab("Enrichment Index") +
    ggplot2::xlab("Structure Index")
  p
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object mf or other types data.
#' @param kei Adjust the proportion of the vertical axis of the graph.
#' @param ksi Adjust the proportion of the abscissa of the graph.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname mf
#' @name mf
#' @aliases nem_plot,mf-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("mf"), function(object, kei = 1, ksi = 1, ...){
  # object = hehe2
  # kei = 3
  # ksi = 3
  meta = object@result
  meta2 = stats::na.omit(meta)
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2])) |>
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output$meanfufp = (output$meanefp * output$meansfp) / 2
  output$meanfufp_ = (output$meanfufp/sum(output$meanfufp))*100
  output$meanei0.5p = output$meanei + 0.5 * output$meanefp/kei
  output$meanei0.5n = output$meanei - 0.5 * output$meanefp/kei
  output$meansi0.5p = output$meansi + 0.5 * output$meansfp/ksi
  output$meansi0.5n = output$meansi - 0.5 * output$meansfp/ksi
  output1 = output[ , c(names(output)[1], "meanei", "meansi")]
  output2 = output[ , c(names(output)[1], "meanei", "meansi0.5p")]
  colnames(output2) = colnames(output1)
  output3 = output[ , c(names(output)[1], "meanei", "meansi0.5n")]
  colnames(output3) = colnames(output1)
  output4 = output[ , c(names(output)[1], "meanei0.5p", "meansi")]
  colnames(output4) = colnames(output1)
  output5 = output[ , c(names(output)[1],"meanei0.5n", "meansi")]
  colnames(output5) = colnames(output1)
  zoutput = rbind(output1, output5, output3, output4, output2)
  output6 = output[ , c(names(output)[1], "meanfufp_")]
  output6$meanfufp_ = round(output6$meanfufp_, 2)
  output6$meanfufp_ = as.character(output6$meanfufp_)
  output6$meanfufp_ = paste0(output6$meanfufp_,"%")
  zoutput = merge(zoutput, output6, by = names(zoutput)[1])
  zoutput[[names(zoutput)[1]]] = paste(zoutput[[names(zoutput)[1]]], zoutput$meanfufp, sep = "_")
  zoutputsub = zoutput |> dplyr::group_by(zoutput[[names(zoutput)[1]]]) |> dplyr::slice(-1)
  # return(zoutputsub)
  p <- ggplot2::ggplot(zoutput, ggplot2::aes(x = meansi, y = meanei, color = !!rlang::sym(names(zoutput)[1]))) + ggplot2::geom_point(size = 3) +
    ggplot2::xlab("Structure index") + ggplot2::ylab("Enrichment index") + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) + ggplot2::theme_test() +
    ggplot2::geom_hline(yintercept = c(0, 50, 100)) + ggplot2::geom_vline(xintercept = c(0, 50, 100)) + ggplot2::geom_polygon(data = zoutputsub, ggplot2::aes(x = meansi, y = meanei, fill = !!rlang::sym(names(zoutput)[1])), alpha = 0.2)
  p
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object mf or other types data.
#' @param kei Adjust the proportion of the vertical axis of the graph.
#' @param ksi Adjust the proportion of the abscissa of the graph.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname mf2
#' @name mf2
#' @aliases nem_plot,mf2-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("mf2"), function(object, kei = 1, ksi = 1, ...){
  # object = hehe2
  # kei = 3
  # ksi = 3
  meta = object@result
  meta2 = stats::na.omit(meta)
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2])) |>
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output$meanfufp = (output$meanefp * output$meansfp) / 2
  output$meanfufp_ = (output$meanfufp/sum(output$meanfufp))*100
  output$meanfufp_ = round(output$meanfufp_, 2)
  output$meanfufp_ = as.character(output$meanfufp_)
  output$meanfufp_ = paste0(output$meanfufp_,"%")
  output1 = output$meanfufp_
  names(output1) = output[[1]]
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[3])) |>
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output$meanfufp = (output$meanefp * output$meansfp) / 2
  output$meanfufp_ = (output$meanfufp/sum(output$meanfufp))*100
  output$meanfufp_ = round(output$meanfufp_, 2)
  output$meanfufp_ = as.character(output$meanfufp_)
  output$meanfufp_ = paste0(output$meanfufp_,"%")
  output2 = output$meanfufp_
  names(output2) = output[[1]]
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2]), !!rlang::sym(names(meta2)[3])) |>
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output[[1]] <- sapply(output[[1]], function(x) {
    if (x %in% names(output1)) {
      paste0(x, "_", output1[x])
    } else {
      x
    }
  })
  output[[2]] <- sapply(output[[2]], function(x) {
    if (x %in% names(output2)) {
      paste0(x, "_", output2[x])
    } else {
      x
    }
  })
  output$meanei0.5p = output$meanei + 0.5 * output$meanefp/kei
  output$meanei0.5n = output$meanei - 0.5 * output$meanefp/kei
  output$meansi0.5p = output$meansi + 0.5 * output$meansfp/ksi
  output$meansi0.5n = output$meansi - 0.5 * output$meansfp/ksi
  output1 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi")]
  output2 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi0.5p")]
  colnames(output2) = colnames(output1)
  output3 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi0.5n")]
  colnames(output3) = colnames(output1)
  output4 = output[ , c(names(output)[1], names(output)[2], "meanei0.5p", "meansi")]
  colnames(output4) = colnames(output1)
  output5 = output[ , c(names(output)[1],names(output)[2], "meanei0.5n", "meansi")]
  colnames(output5) = colnames(output1)
  zoutput = rbind(output1, output5, output3, output4, output2)
  zoutputsub = zoutput |> dplyr::group_by(zoutput[[names(zoutput)[1]]], zoutput[[names(zoutput)[2]]]) |> dplyr::slice(-1)
  # return(zoutputsub)
  p <- ggplot2::ggplot(zoutput, ggplot2::aes(x = meansi, y = meanei, color = !!rlang::sym(names(zoutput)[1]), shape = !!rlang::sym(names(zoutput)[2]))) + ggplot2::geom_point(size = 3) +
    ggplot2::xlab("Structure index") + ggplot2::ylab("Enrichment index") + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) + ggplot2::theme_test() +
    ggplot2::geom_hline(yintercept = c(0, 50, 100)) + ggplot2::geom_vline(xintercept = c(0, 50, 100)) + ggplot2::geom_polygon(data = zoutputsub, ggplot2::aes(x = meansi, y = meanei, fill = !!rlang::sym(names(zoutput)[1])), alpha = 0.2)
  p
})

#' nem_plot
#' @description For visualization of nematode community data.
#' @param object ef or other types data.
#' @return An ggplot object.
#' @rdname ef
#' @name ef
#' @aliases nem_plot,ef-method
#' @import ggplot2
#' @import ggalt
#' @import reshape2
#' @import igraph
#' @import ggraph
#' @export
setMethod("nem_plot", signature("ef"), function(object){
  # object = hehe
  result = object@result
  result = result[,-1]
  result2 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1])) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)))
  result3 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1])) |>
    dplyr::summarise(Use = stats::sd(U)/sqrt(dplyr::n()))
  result4 = merge(result2,result3, by = names(result)[1])
  result4_U = result4[,c(names(result4)[1], "U", "Use")]
  result4_U$U = format(result4_U$U, digits = 2, nsmall = 2)
  result4_U$Use = format(result4_U$Use, digits = 2, nsmall = 2)
  result4_U = result4_U |> dplyr::mutate(U = paste0("U = ", U, " (", "\u00B1", Use, ")"))
  result4_nodes = result4[,c(names(result4)[1], "OM", "BM", "HM", "FM")]
  result4_nodes$R = 600
  names(result4_nodes) = c(names(result4_nodes)[1], "Omnivores_carnivores", "Bacterivores", "Herbivores", "Fungivores", "Resources")
  result4_nodes_long = reshape2::melt(result4_nodes,id.vars = names(result4_nodes)[1], variable.name = "Feeding", value.name = "Fresh_biomass")
  result4_edges = result4[,c(names(result4)[1],"fbo", "fho", "ffo", "frb", "frh", "frf")]
  result4_edges_long = reshape2::melt(result4_edges,id.vars = names(result4_nodes)[1], variable.name = "group", value.name = "Energy_flow")
  result4_edges_long = result4_edges_long |>
    dplyr::mutate(group = dplyr::case_when(
      group == "fbo" ~ "Bacterivores-Omnivores_carnivores",
      group == "fho" ~ "Herbivores-Omnivores_carnivores",
      group == "ffo" ~ "Fungivores-Omnivores_carnivores",
      group == "frb" ~ "Resources-Bacterivores",
      group == "frh" ~ "Resources-Herbivores",
      group == "frf" ~ "Resources-Fungivores",
      TRUE ~ group  # 保留其他未匹配的值
    ))
  result4_edges_long = result4_edges_long |>
    tidyr::separate(group, into = c("from1", "to1"), sep = "-")
  result4_edges_long = result4_edges_long |> dplyr::mutate(from = paste0(!!rlang::sym("from1"), !!rlang::sym(names(result4)[1])))
  result4_edges_long = result4_edges_long |> dplyr::mutate(to = paste0(!!rlang::sym("to1"), !!rlang::sym(names(result4)[1])))
  result4_edges_long = result4_edges_long |> dplyr::select(!!rlang::sym("from"), !!rlang::sym("to"), dplyr::everything())
  nodes <- data.frame(
    id = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores"),
    Feeding = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores")
  )
  # nodes = dplyr::bind_rows(replicate(nrow(result4), nodes, simplify = FALSE))
  # nodes[[names(result4)[1]]] = sort(rep(result4[[1]], times = 5))
  nodes = merge(nodes, result4_nodes_long, by = "Feeding")
  nodes = nodes |> dplyr::mutate(id = paste0(!!rlang::sym("id"), !!rlang::sym(names(result4)[1])))
  nodes$Feeding = factor(nodes$Feeding, levels = c("Bacterivores", "Fungivores", "Herbivores", "Resources", "Omnivores_carnivores"))
  result4_U = result4_U[, -which(names(result4_U) == "Use")]
  nodes = merge(nodes, result4_U, by = names(result)[1])
  # nodes = merge(nodes, result4_nodes_long, by = c(names(result4)[1], "Feeding"), all = TRUE)
  nodes = dplyr::select(nodes, id, dplyr::everything())
  nodes$Fresh_biomass = round(nodes$Fresh_biomass,2)
  nodes$label = nodes$Fresh_biomass
  nodes = nodes |> dplyr::mutate(label = ifelse(Feeding == "Resources", "R", label))
  order = rep(c("Bacterivores", "Fungivores", "Herbivores", "Omnivores_carnivores", "Resources"), times = nrow(nodes)/5)
  positions <- integer(length(order))
  used_indices <- logical(length(order))
  for (i in 1:length(order)){
    for(j in 1:length(order)){
      if(order[i] == nodes[["Feeding"]][j] && !used_indices[j]){
        positions[i] = j
        used_indices[j] <- TRUE
        break
      }
    }
  }
  nodes = nodes[positions,]
  # edges <- data.frame(
  #   from = c("Resources", "Resources", "Resources", "Bacterivores", "Herbivores", "Fungivores"),
  #   to = c("Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores", "Omnivores_carnivores", "Omnivores_carnivores"),
  #   group = c("Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores", "Omnivores_carnivores", "Omnivores_carnivores")
  # )
  # edges = dplyr::bind_rows(replicate(nrow(result4), edges, simplify = FALSE))
  # edges[[names(result4)[1]]] = sort(rep(result4[[1]], times = 6))
  # edges = edges |> dplyr::mutate(from = paste0(!!rlang::sym("from"), !!rlang::sym(names(result4)[1])))
  # edges = edges |> dplyr::mutate(to = paste0(!!rlang::sym("to"), !!rlang::sym(names(result4)[1])))
  # edges = merge(edges, result4_U, by = names(result)[1])
  # edges = merge(edges, result4_edges_long, by = c("from", "to"), all = TRUE)
  edges = result4_edges_long
  edges$Energy_flow = round(edges$Energy_flow, 2)
  graph = tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE, node_key = "id")

  # graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  layout_matrix <- matrix(c(
    0, 3,
    3, 0,
    1.5, 1.5,
    3, 3,
    0, 0
  ), ncol = 2, byrow = TRUE)
  formu = stats::as.formula(paste0("~", names(result4)[1]))
  p3 = ggraph::ggraph(graph, layout = layout_matrix) +
    ggraph::geom_edge_fan(ggplot2::aes(colour = to1, width = Energy_flow, label = Energy_flow), family = NA, show.legend = FALSE) +
    ggraph::scale_edge_width(range=c(0.5, 4)) +
    ggplot2::geom_text(ggplot2::aes(x = 2, y = 0.5, label = U),family = NA, data = nodes)+
    ggraph::geom_node_point(ggplot2::aes(colour = Feeding, size = Fresh_biomass)) +
    ggraph::geom_node_text(aes(label = label), repel = TRUE) +
    ggraph::theme_graph(foreground = 'steelblue', fg_text_colour = 'white', base_family = NA) +
    ggplot2::scale_size(range = c(5,13)) +
    ggplot2::facet_wrap(formu, scales = "free")
  p3
})

#' nem_plot
#' @description For visualization of nematode community data.
#' @param object ef2 or other types data.
#' @return An ggplot object.
#' @rdname ef2
#' @name ef2
#' @aliases nem_plot,ef2-method
#' @import ggplot2
#' @import ggalt
#' @import reshape2
#' @import igraph
#' @import ggraph
#' @export
setMethod("nem_plot", signature("ef2"), function(object){
  # object = hehe
  result = object@result
  result = result[,-1]
  result2 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1]),!!rlang::sym(names(result)[2])) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)))
  result3 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1]),!!rlang::sym(names(result)[2])) |>
    dplyr::summarise(Use = stats::sd(U)/sqrt(dplyr::n()))
  result4 = merge(result2,result3, by = c(names(result)[1], names(result)[2]))
  result4_U = result4[,c(names(result4)[1], names(result4)[2], "U", "Use")]
  result4_U$U = format(result4_U$U, digits = 2, nsmall = 2)
  result4_U$Use = format(result4_U$Use, digits = 2, nsmall = 2)
  result4_U = result4_U |> dplyr::mutate(U = paste0("U = ", U, " (", "\u00B1", Use, ")"))
  result4_nodes = result4[,c(names(result4)[1], names(result4)[2], "OM", "BM", "HM", "FM")]
  result4_nodes$R = 600
  names(result4_nodes) = c(names(result4_nodes)[1], names(result4_nodes)[2], "Omnivores_carnivores", "Bacterivores", "Herbivores", "Fungivores", "Resources")
  result4_nodes_long = reshape2::melt(result4_nodes,id.vars = c(names(result4_nodes)[1], names(result4_nodes)[2]), variable.name = "Feeding", value.name = "Fresh_biomass")
  result4_edges = result4[,c(names(result4)[1],names(result4)[2], "fbo", "fho", "ffo", "frb", "frh", "frf")]
  result4_edges_long = reshape2::melt(result4_edges,id.vars = c(names(result4_nodes)[1],names(result4_nodes)[2]), variable.name = "group", value.name = "Energy_flow")
  result4_edges_long = result4_edges_long |>
    dplyr::mutate(group = dplyr::case_when(
      group == "fbo" ~ "Bacterivores-Omnivores_carnivores",
      group == "fho" ~ "Herbivores-Omnivores_carnivores",
      group == "ffo" ~ "Fungivores-Omnivores_carnivores",
      group == "frb" ~ "Resources-Bacterivores",
      group == "frh" ~ "Resources-Herbivores",
      group == "frf" ~ "Resources-Fungivores",
      TRUE ~ group  # 保留其他未匹配的值
    ))
  result4_edges_long = result4_edges_long |>
    tidyr::separate(group, into = c("from1", "to1"), sep = "-")
  result4_edges_long = result4_edges_long |> dplyr::mutate(from = paste0(!!rlang::sym("from1"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  result4_edges_long = result4_edges_long |> dplyr::mutate(to = paste0(!!rlang::sym("to1"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  result4_edges_long = result4_edges_long |> dplyr::select(!!rlang::sym("from"), !!rlang::sym("to"), dplyr::everything())
  nodes <- data.frame(
    id = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores"),
    Feeding = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores")
  )
  # nodes = dplyr::bind_rows(replicate(nrow(result4), nodes, simplify = FALSE))
  # nodes[[names(result4)[1]]] = sort(rep(result4[[1]], times = 5))
  # nodes[[names(result4)[2]]] = rep(unique(result4[[2]]), times = length(unique(result4[[1]]))*5)
  nodes = merge(nodes, result4_nodes_long, by = "Feeding")
  nodes = nodes |> dplyr::mutate(id = paste0(!!rlang::sym("id"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  nodes$Feeding = factor(nodes$Feeding, levels = c("Bacterivores", "Fungivores", "Herbivores", "Resources", "Omnivores_carnivores"))
  result4_U = result4_U[, -which(names(result4_U) == "Use")]
  nodes = merge(nodes, result4_U, by = c(names(result)[1], names(result)[2]))
  # nodes = merge(nodes, result4_nodes_long, by = c(names(result4)[1], names(result4)[2], "Feeding"), all = TRUE)
  nodes = dplyr::select(nodes, id, dplyr::everything())
  nodes$Fresh_biomass = round(nodes$Fresh_biomass,2)
  nodes$label = nodes$Fresh_biomass
  nodes = nodes |> dplyr::mutate(label = ifelse(Feeding == "Resources", "R", label))
  order = rep(c("Bacterivores", "Fungivores", "Herbivores", "Omnivores_carnivores", "Resources"), times = nrow(nodes)/5)
  positions <- integer(length(order))
  used_indices <- logical(length(order))
  for (i in 1:length(order)){
    for(j in 1:length(order)){
      if(order[i] == nodes[["Feeding"]][j] && !used_indices[j]){
        positions[i] = j
        used_indices[j] <- TRUE
        break
      }
    }
  }
  nodes = nodes[positions,]
  # edges = dplyr::bind_rows(replicate(nrow(result4), edges, simplify = FALSE))
  # edges[[names(result4)[1]]] = sort(rep(result4[[1]], times = 6))
  # edges[[names(result4)[2]]] = rep(unique(result4[[2]]), times = length(unique(result4[[1]]))*6)
  # edges = edges |> dplyr::mutate(from = paste0(!!rlang::sym("from"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  # edges = edges |> dplyr::mutate(to = paste0(!!rlang::sym("to"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  # edges = merge(edges, result4_U, by = c(names(result)[1], names(result)[2]))
  # edges = merge(edges, result4_edges_long, by = c("from", "to"), all = TRUE)
  edges = result4_edges_long
  edges$Energy_flow = round(edges$Energy_flow, 2)
  graph = tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE, node_key = "id")
  # graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  layout_matrix <- matrix(c(
    0, 3,
    3, 0,
    1.5, 1.5,
    3, 3,
    0, 0
  ), ncol = 2, byrow = TRUE)
  formu = stats::as.formula(paste0(names(result4)[2], "~", names(result4)[1]))
  p3 = ggraph::ggraph(graph, layout = layout_matrix) +
    ggraph::geom_edge_fan(ggplot2::aes(colour = to1, width = Energy_flow, label = Energy_flow), family = NA, show.legend = FALSE) +
    ggraph::scale_edge_width(range=c(0.5, 4)) +
    ggplot2::geom_text(ggplot2::aes(x = 2, y = 0.5, label = U),family = NA, data = nodes)+
    ggraph::geom_node_point(ggplot2::aes(colour = Feeding, size = Fresh_biomass)) +
    ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE) +
    ggraph::theme_graph(foreground = 'steelblue', fg_text_colour = 'white', base_family = NA) +
    ggplot2::scale_size(range = c(5,13)) +
    ggplot2::facet_grid(formu, scales = "free")
  p3
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object ter or other types data.
#' @param type Select the ternary graph type, cp or feeding.
#' @return An ggplot object.
#' @rdname ter
#' @name ter
#' @aliases nem_plot,ter-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("ter"), function(object, type){
  # object = hehe
  type = deparse(substitute(type))
  if (type == "cp"){
  result = object@result
  base = ggtern::ggtern(data=result,ggplot2::aes(x=`cp1% (Enrichment)`,y=`cp3-5% (Stability)`,z=`cp2% (Stress)`))
  p3 = base +
    ggplot2::geom_point(ggplot2::aes(color=!!rlang::sym(names(result)[2]))) +
    ggtern::theme_rgbw()
  p4 = p3 + ggplot2::labs(x = "cp1%\n(Enrichment)", y = "cp3-5%\n(Stability)", z = "cp2%\n(Stress)")
  } else if (type == "feeding") {
    result = object@result
    base = ggtern::ggtern(data=result,ggplot2::aes(x=`Herbivorous nematodes%`,y=`Bacteria-feeding nematodes%`,z=`Fungus-feeding nematodes%`))
    p3 = base +
      ggplot2::geom_point(ggplot2::aes(color=!!rlang::sym(names(result)[2]))) +
      ggtern::theme_rgbw()
    p4 = p3 + ggplot2::labs(x = "Herbivorous%", y = "Bacteria%", z = "Fungus%")
  }
  p4
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object ter2 or other types data.
#' @param type Select the ternary graph type, cp or feeding.
#' @return An ggplot object.
#' @rdname ter2
#' @name ter2
#' @aliases nem_plot,ter2-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("ter2"), function(object, type){
  # object = hehe
  type = deparse(substitute(type))
  if (type == "cp"){
  result = object@result
  base = ggtern::ggtern(data=result,ggplot2::aes(x=`cp1% (Enrichment)`,y=`cp3-5% (Stability)`,z=`cp2% (Stress)`))
  p3 = base +
    ggplot2::geom_point(ggplot2::aes(color=!!rlang::sym(names(result)[2]), shape = !!rlang::sym(names(result)[3]))) +
    ggtern::theme_rgbw()
  p4 = p3 + ggplot2::labs(x = "cp1%\n(Enrichment)", y = "cp3-5%\n(Stability)", z = "cp2%\n(Stress)")
  } else if (type == "feeding") {
    result = object@result
    base = ggtern::ggtern(data=result,ggplot2::aes(x=`Herbivorous nematodes%`,y=`Bacteria-feeding nematodes%`,z=`Fungus-feeding nematodes%`))
    p3 = base +
      ggplot2::geom_point(ggplot2::aes(color=!!rlang::sym(names(result)[2]), shape = !!rlang::sym(names(result)[3]))) +
      ggtern::theme_rgbw()
    p4 = p3 + ggplot2::labs(x = "Herbivorous%", y = "Bacteria%", z = "Fungus%")
  }
  p4
})
