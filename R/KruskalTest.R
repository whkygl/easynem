#' KruskalTest
#' @description Kruskal-Wallis test was performed for grouped data.
#' @param data easynem type data.
#' @param .group The group variable.
#' @param y Dependent variable.
#' @param exact a logical indicating whether an exact p-value should be computed.
#' @param sort logical. If sort groups by median dependent variable values.
#' @param .method method for correcting p-values for multiple comparisons.
#' @param ... Other parameters for KruskalTest.
#' @return An compare object.
#' @export
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
  if(sort) g <- stats::reorder(g, y, FUN=stats::median)                           
  groups <- levels(g)
  k <- nlevels(g)
  getstats <- function(x)(c(N = length(x), Median = stats::median(x),      
                            MAD = stats::mad(x)))
  sumstats <- t(stats::aggregate(y, by=list(g), FUN=getstats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  kw <- stats::kruskal.test(meta[[y2]]~meta[[.group]], data=meta, ...)                             
  wmc <- NULL
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      y1 <- y[g==groups[i]]
      y2 <- y[g==groups[j]] 
      test <- stats::wilcox.test(y1, y2, exact=exact)
      r <- data.frame(Group.1=groups[i], Group.2=groups[j], 
                      W=test$statistic[[1]], p=test$p.value)
      wmc <- rbind(wmc, r)
    }
  }
  wmc$p <- stats::p.adjust(wmc$p, method=method)
  
  
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