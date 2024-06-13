#' WilcoxTest
#' @description Wilcoxon test was performed for grouped data.
#' @param data easynem type data.
#' @param .group The group variable.
#' @param y Dependent variable.
#' @param ... Other parameters for WilcoxTest.
#' @return An compare object.
#' @export
WilcoxTest <- function(data, .group, y, ...){
    .compare = methods::new("compare")
    meta = as.data.frame(data@meta)
    meta = meta[,c(names(meta)[1], .group, y)]
    row.names(meta) = meta[,1]
    y1 = meta[meta[,2] == unique(meta[,2])[1],3]        
    y2 = meta[meta[,2] == unique(meta[,2])[2],3]  
    result = stats::wilcox.test(y1, y2, ...)
    .compare@result = result
    .compare@meta = meta
    .compare@temp = c("WilcoxTest")
    return(.compare)
}