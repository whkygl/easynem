#' Beta diversity analysis, generating beta-class (single factor)
#'
#' The \code{calc_beta()} is used to perform beta diversity analysis and create
#' \code{\link{beta-class}}. This function is only applicable to single factor
#' analysis, see \code{\link{calc_beta2}} for a two-factor version of the
#' function.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_pca <- nem |> calc_beta(pca, Treatments, method = "bray")
#' ```
#'
#' @usage calc_beta(data, type, .group, method, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param type Types of beta diversity analysis (\code{pca}, \code{pcoa} or \code{nmds}).
#' @param .group Treatment factors that need to be compared.
#' @param method Dissimilarity index, partial match to `"manhattan"`, `"euclidean"`,
#' `"canberra"`, `"clark"`, `"bray"`, `"kulczynski"`, `"jaccard"`, `"gower"`,
#' `"altGower"`, `"morisita"`, `"horn"`, `"mountford"`, `"raup"`, `"binomial"`,
#' `"chao"`, `"cao"`, `"mahalanobis"`, `"chisq"`, `"chord"`, `"hellinger"`,
#' `"aitchison"`, or `"robust.aitchison"`. See \code{\link[vegan]{vegdist}}.
#' @param ... Other parameters for \code{\link[stats]{cmdscale}}, \code{\link[vegan]{vegdist}}
#' and \code{\link[vegan]{adonis2}}.
#'
#' @return A \code{\link{beta-class}} for storing beta diversity analysis results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_alpha}}, \code{\link{calc_nemindex}}, \code{\link{calc_funguild}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_pcoa <- nem |> calc_beta(pcoa, Treatments, method = "bray")
#' show(nem_pcoa)
#' nem_nmds <- nem |> calc_beta(nmds, Treatments, method = "bray")
#' show(nem_nmds)
calc_beta <- function(data, type, .group, method, ...){
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
      adonis = paste0("Adonis: R=",round(div$R2,3), "; p=",div$`Pr(>F)`)
      temp = c(x,y,adonis)
      .beta@temp = temp
      .beta@meta = tibble::as_tibble(pcoa_result)
      .beta@result = adonis[1]
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
      .beta@result = adonis[1]
    } else if (type == 'pca'){
      pca = stats::prcomp(otu2, ...)
      formula_str = paste("otu2~", .group)
      formula_obj = stats::as.formula(formula_str)
      div = vegan::adonis2(formula_obj, data = meta, permutations = 999, ...)
      adonis = paste0("Adonis: R=",round(div$R2,3), "; p=",div$`Pr(>F)`)
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
      .beta@result = adonis[1]
    } else{
      stop("type should be one of 'pcoa', 'nmds' and 'pca'")
    }
  return(.beta)
}
