#' LSD test for grouped data.
#' @description Multiple comparisons of treatments by means of LSD and a grouping of treatments. The level by alpha default is 0.05. Returns p-values adjusted using one of several methods.
#' @param data easynem type data.
#' @param .group The group variable.
#' @param y Dependent variable.
#' @param ... Other parameters for LSD.test.
#' @return An compare object.
#' @export
LSD <- function(data, .group, y, ...){
  .compare = methods::new("compare")
  meta = as.data.frame(data@meta)
  meta = meta[, c(names(meta)[1], .group, y)]
  row.names(meta) = meta[,1]
  formula_str <- paste(y, "~", .group)
  formula <- stats::as.formula(formula_str)
  fit = stats::aov(formula, data = meta, ...)
  .compare@meta = meta
  .compare@result$`Fit an Analysis of Variance Model` = summary(fit)
  .compare@temp = c("LSD")
  lsd = agricolae::LSD.test(fit, .group, ...)
  .compare@result$LSD = lsd
  return(.compare)
}