#' HSD2
#' @description HSD test was performed for grouped data.
#' @param data easynem type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @param y Dependent variable.
#' @param ... Other parameters for HSD2.
#' @return An compare2 object.
#' @export
HSD2 <- function(data, .group1, .group2, y, ...){
  .compare2 = methods::new("compare2")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group1, .group2, y)]
  .compare2@meta = meta
  .compare2@temp = c("HSD2")
  perform_lsd_test <- function(data2, ...) {
    formu = paste0(y, "~", .group1)
    formu = stats::as.formula(formu)
    lsd_test = agricolae::HSD.test(stats::aov(formu, data = data2, ...), .group1, ...)
    result1 = lsd_test$means
    result1 = result1[,-1]
    result1$group = rownames(result1)
    result2 = lsd_test$groups
    result2$group = rownames(result2)
    result = merge(result1, result2, by = "group")
    return(result)
  }
  results_list = lapply(split(meta, meta[[.group2]]), perform_lsd_test)
  results <- do.call(rbind, Map(cbind, group2 = names(results_list), results_list))
  names(results)[names(results) == "group2"] = .group2
  names(results)[names(results) == "groups"] = "label"
  names(results)[names(results) == "group"] = .group1
  .compare2@result = results
  return(.compare2)
}