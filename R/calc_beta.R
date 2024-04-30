#' An S4 class to store beta diversity results.
#' @slot meta A data frame of meta data.
#' @slot result A data frame of pairwise comparison results.
#' @slot temp A character vector of the difference comparison.
methods::setClass("beta",
                  slots = list(
                    meta = "data.frame",
                    result = "data.frame",
                    temp = "character"
                  ))
methods::setMethod("show", "beta", function(object){
  cat("This is an beta object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' calc_beta
#' @description Calculate beata diversity and generate results for multiple comparisons between treatments.
#' @param data easynem type data.
#' @param type pca, pcoa or nmds.
#' @param .group The group variable.
#' @param method The method of calculating the distance matrix.
#' @param ... Other parameters for cmdscale and vegdist functions.
#' @return An beta object.
#' @export
calc_beta <- function(data, type, .group, method, ...){
  p_list = c("pairwiseAdonis")
  for (p in p_list) {
    if (!requireNamespace(p)) {
      remotes::install_github(p)
    }
  }
    type = deparse(substitute(type))
    .group = deparse(substitute(.group))
    method = deparse(substitute(method))
    .beta = methods::new("beta")
    otu = as.data.frame(data@tab)
    row.names(otu) = otu[,1]
    otu = otu[,-1]
    otu2 = t(otu)
    meta = as.data.frame(data@meta)
    meta = meta[,c(1, which(names(meta) == .group))]
    row.names(meta) = meta[,1]
    dist = vegan::vegdist(t(otu), binary = FALSE, scale= TRUE,center = TRUE, ...)
    if (type == 'pcoa'){
      pcoa = stats::cmdscale(dist, k = 3, eig =TRUE, ...)
      pcoa_points = as.data.frame(pcoa$points)
      sum_eig = sum(pcoa$eig)
      eig_percent = round(pcoa$eig/sum_eig*100,1)
      colnames(pcoa_points) <- paste0("PCoA", 1:3)
      inter = intersect(rownames(meta), rownames(pcoa_points))
      meta = meta[inter,]
      pcoa_points = pcoa_points[inter,]
      pcoa_result = cbind(meta, pcoa_points)
      x=paste("PCoA 1 (", eig_percent[1], "%)", sep="")
      y=paste("PCoA 2 (", eig_percent[2], "%)", sep="")
      formula_str = paste("otu2~", .group)
      formula_obj = stats::as.formula(formula_str)
      div = vegan::adonis2(formula_obj, data = meta, permutations = 999, ...)
      adonis = paste0("adonis: R2=",round(div$R2,3), "; p=",div$`Pr(>F)`)
      temp = c(x,y,adonis)
      .beta@temp = temp
      .beta@meta = tibble::as_tibble(pcoa_result)
    } else if (type == 'nmds'){
      utils::capture.output(df_nmds <-  vegan::metaMDS(dist, k = 3))
      stress = df_nmds$stress
      nmds_points = as.data.frame(df_nmds$points)
      names(nmds_points)[1:2] <- c('NMDS1', 'NMDS2')
      inter = intersect(rownames(meta), rownames(nmds_points))
      meta = meta[inter,]
      nmds_points = nmds_points[inter,]
      nmds_result = cbind(meta, nmds_points)
      adonis = paste('Stress=',round(stress, 3))
      .beta@temp = adonis
      .beta@meta = tibble::as_tibble(nmds_result)
    } else if (type == 'pca'){
      pca = stats::prcomp(otu2, ...)
      formula_str = paste("otu2~", .group)
      formula_obj = stats::as.formula(formula_str)
      div = vegan::adonis2(formula_obj, data = meta, permutations = 999, ...)
      adonis = paste0("adonis: R2=",round(div$R2,3), "; p=",div$`Pr(>F)`)
      pca_points = as.data.frame(pca$x)
      inter = intersect(rownames(meta), rownames(pca_points))
      meta = meta[inter,]
      pca_points = pca_points[inter,]
      pca_result = cbind(meta, pca_points)
      summ1 = summary(pca)
      x = paste0("PCA1(",round(summ1$importance[2,1]*100,2),"%)")
      y = paste0("PCA2(",round(summ1$importance[2,2]*100,2),"%)")
      temp = c(x, y, adonis)
      .beta@temp = temp
      .beta@meta = tibble::as_tibble(pca_result)
    } else{
      stop("type should be one of 'pcoa', 'nmds' and 'pca'")
    }
  pair_adonis = pairwiseAdonis::pairwise.adonis(x=otu2, factors = meta[[.group]], sim.function = "vegdist",
                                                sim.method = method, p.adjust = "BH", reduce = NULL, perm = 999)
  .beta@result = tibble::as_tibble(pair_adonis)
  return(.beta)
}