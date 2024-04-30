rm(list = ls())
# library(tidyverse)
# otu <- read_csv("bacotu.csv")
# meta <- read_csv("meta.csv")
methods::setClass("beta2",
                  slots = list(
                    meta = "data.frame",
                    result = "data.frame",
                    temp = "character"
                  ))
methods::setMethod("show", "beta2", function(object){
  cat("This is an beta2 object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
calc_beta2 <- function(data, type, .group1, .group2, method, ...){
  p_list = c("pairwiseAdonis")
  for (p in p_list) {
    if (!requireNamespace(p)) {
      remotes::install_github(p)
    }
  }
  type = deparse(substitute(type))
#   .group1 = "con_crop"
#   .group2 = "season"
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  method = deparse(substitute(method))
  .beta = methods::new("beta2")
  otu = as.data.frame(data@tab)
  row.names(otu) = otu[,1]
  otu = otu[,-1]
  otu2 = t(otu)
  meta = as.data.frame(data@meta)
  # select函数选择meta的第一列，.group1列和.group2列
  meta = meta |> dplyr::select(tidyr::all_of(1),tidyr::all_of(.group1),tidyr::all_of(.group2))
  row.names(meta) = meta[,1]
  dist = vegan::vegdist(t(otu), binary = FALSE, scale= TRUE,center = TRUE, ...)
  if (type == "pcoa"){
    pcoa = cmdscale
  }
}
