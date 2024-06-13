rm(list = ls())
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"), meta = easynem_example("meta.csv"))
calc_compare <- function(data, .group, y, method, ...){
  .group = deparse(substitute(.group))
  y = deparse(substitute(y))
  result = method(data, .group, y, ...)
  return(result)
}
KruskalTest <- function(data, .group, y, exact=FALSE, sort=TRUE,               
                        .method=c("holm", "hochberg", "hommel", "bonferroni",      
                                 "BH", "BY", "fdr", "none"), ...){
  .compare = methods::new("compare")
  y2 = y
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group, y)]
  row.names(meta) = meta[,1]
  method <- match.arg(.method)
  df = meta
  y <- df[[3]]
  g <- as.factor(df[[2]])
  vnames <- names(df)
  if(sort) g <- reorder(g, y, FUN=median)                           
  groups <- levels(g)
  k <- nlevels(g)
  getstats <- function(x)(c(N = length(x), Median = median(x),      
                            MAD = mad(x)))
  sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  kw <- stats::kruskal.test(meta[[y2]]~meta[[.group]], data=meta, ...)                             
  wmc <- NULL
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      y1 <- y[g==groups[i]]
      y2 <- y[g==groups[j]] 
      test <- wilcox.test(y1, y2, exact=exact)
      r <- data.frame(Group.1=groups[i], Group.2=groups[j], 
                      W=test$statistic[[1]], p=test$p.value)
      # note the [[]] to return a single number
      wmc <- rbind(wmc, r)
    }
  }
  wmc$p <- p.adjust(wmc$p, method=method)
  
  
  data <- data.frame(y, g)                                    
  names(data) <- c(vnames[3],vnames[2])
  results <- list(CALL = match.call(), 
                  data=data,
                  sumstats=sumstats, kw=kw, 
                  method=method, wmc=wmc, vnames=vnames)
  .compare@result = results
  .compare@meta = meta
  .compare@temp = c("KruskalTest")
  return(.compare)
}
hehe <- calc_compare(bac, con_crop, pH, method = KruskalTest)
hehe
nem_plot <- function(object, type = 1, add, ...) {
  # object = hehe
  meta = object@meta
  temp = object@temp
  if (temp == "KruskalTest") {
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
  p  
}
p = nem_plot(hehe, type = 1)
p
