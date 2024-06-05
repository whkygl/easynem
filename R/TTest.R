#' An S4 class to store multiple comparisons results.
#' @slot meta A data frame of meta data.
#' @slot result A data frame of multiple comparisons results.
#' @slot temp A character vector of the difference comparison.
methods::setClass("compare",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY",
                    temp = "character"
                  ))
methods::setMethod("show", "compare", function(object){
  cat("This is an compare object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' TTest
#' @description t test was performed for grouped data.
#' @param data easynem type data.
#' @param .group The group variable.
#' @param y Dependent variable.
#' @param ... Other parameters for TTest.
#' @return An compare object.
#' @export
TTest <- function(data, .group, y, ...){
    .compare = methods::new("compare")
    meta = as.data.frame(data@meta)
    meta = meta[,c(1, which(names(meta) %in% c(.group, y)))]
    row.names(meta) = meta[,1]
    y1 = meta[meta[,2] == unique(meta[,2])[1],3]        
    y2 = meta[meta[,2] == unique(meta[,2])[2],3]  
    result = stats::t.test(y1, y2, ...)
    .compare@result = result
    .compare@meta = meta
    .compare@temp = c("TTest")
    return(.compare)
}
