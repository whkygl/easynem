#' Compute Tukey Honest Significant Differences (two-factor)
#'
#' The \code{HSD2()} is used to Compute Tukey Honest Significant Differences for
#' grouped data and create \code{\link{compare2-class}}. This function is only
#' applicable to two-factor analysis, see \code{\link{HSD}} for a single factor
#' version of the function.
#'
#' To facilitate code interpretation, It is recommended to use this function in
#' conjunction with the \code{\link{calc_compare2}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = HSD2)
#' ```
#'
#' @usage HSD2(data, .group1, .group2, y, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group1 Grouping variables factor 1.
#' @param .group2 Grouping variables factor 2.
#' @param y Dependent variable (numeric data).
#' @param ... Other parameters for \code{\link[stats]{TukeyHSD}}.
#'
#' @return An \code{\link{compare2-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest2}},
#' \code{\link{TTest}}, \code{\link{WilcoxTest2}}, \code{\link{WilcoxTest}},
#' \code{\link{KruskalTest2}}, \code{\link{KruskalTest}}, \code{\link{LSD2}}, \code{\link{LSD}},
#' \code{\link{HSD}}.
#'
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_test <- nem |>
#'               calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = HSD2)
#' nem_test
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
