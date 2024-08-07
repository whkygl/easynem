library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), 
                tax = easynem_example("bactax.csv"), 
                meta = easynem_example("meta.csv"))
calc_compare2 <- function(data, .group1, .group2, y, method, ...){
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  y = deparse(substitute(y))
  result = method(data, .group1, .group2, y, ...)
  return(result)
}
methods::setClass("compare2",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY",
                    temp = "character"
                  ))
methods::setMethod("show", "compare2", function(object){
  cat("This is an compare2 object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
TTest2 <- function(data, .group1, .group2, y, ...){
  .compare2 = methods::new("compare2")
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group1, .group2, y)]
  name1 = unique(meta[,2])[1]
  name2 = unique(meta[,2])[2]
  .compare2@meta = meta
    meta = meta[,-1]
    meta = meta |> 
      dplyr::group_by(!!rlang::sym(.group2), !!rlang::sym(.group1)) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup()
    meta_wide <- meta |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.group1), values_from = !!rlang::sym(y), names_prefix = paste0(y,"_")) |>
      dplyr::select(-id)
    result = meta_wide |>
      dplyr::group_by(!!rlang::sym(.group2)) |>
      dplyr::do(broom::tidy(stats::t.test(as.vector(.[,2])[[1]], as.vector(.[,3])[[1]], ...)))
    result$group = paste0(name1, "-", name2)
    .compare2@result = result
  .compare2@temp = c("TTest2")
  return(.compare2)
}
hehe <- bac |> filter_name(meta, con_crop %in% c("Y2","Y11")) |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
hehe
hehe <- bac |> filter_name(meta, con_crop %in% c("Y2","Y11")) |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2, paired = TRUE)
hehe
