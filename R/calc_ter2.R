#' An S4 class to store the ternary analysis results.
#' @slot result A data frame of the ternary analysis results.
methods::setClass("ter2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "ter2", function(object){
  cat("This is an ter2 object\n")
  cat("The ternary analysis results is:\n")
  print(object@result)
})
#' calc_ter2
#' @description Calculate ternary analysis between treatments.
#' @param data easynem type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @return An ter2 object.
#' @export
calc_ter2 <- function(data, .group1, .group2){
  # data = bac
  # .group1 = "con_crop"
  # .group2 = "season"
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  .ter = methods::new("ter2")
  cp_tax = data@tax[,c("OTUID", "feeding", "cp_value")]
  cp_tax2 = data@tax[,c("OTUID", "feeding", "GenavgMass")]
  cp_tax$cp_value[cp_tax$feeding == 1] = 0
  cp_tax = cp_tax[,-2]
  tab = data@tab
  meta = data@meta[,c("SampleID", .group1, .group2)]
  tab_cp = merge(tab, cp_tax, by = "OTUID")
  tab_cp2 = merge(tab, cp_tax2, by = "OTUID")
  tab_cp2 = subset(tab_cp2, !is.na(GenavgMass))
  tab_cp2[,2:(ncol(tab_cp2)-2)] = tab_cp2[,2:(ncol(tab_cp2)-2)] * tab_cp2[,ncol(tab_cp2)]
  tab_cp = tab_cp[,-1]
  tab_cp2 = tab_cp2[,-c(1,ncol(tab_cp2))]
  tab_cp = tab_cp |> dplyr::group_by(!!rlang::sym("cp_value")) |> dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE)))
  tab_cp2 = tab_cp2 |> dplyr::group_by(!!rlang::sym("feeding")) |> dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE)))
  tab_cp <- tab_cp |>
    dplyr::mutate(dplyr::across(1, ~ paste0("cp", ., "%")))
  tab_cp2$feeding[tab_cp2$feeding == 1] = "Herbivorous nematodes%"
  tab_cp2$feeding[tab_cp2$feeding == 2] = "Fungus-feeding nematodes%"
  tab_cp2$feeding[tab_cp2$feeding == 3] = "Bacteria-feeding nematodes%"
  tab_cp[, -1] = apply(tab_cp[, -1], 2, function(x) x / sum(x))
  tab_cp2[, -1] = apply(tab_cp2[, -1], 2, function(x) x / sum(x))
  row_range <- 4:6
  col_range <- 2:ncol(tab_cp)
  sums <- apply(tab_cp[row_range, col_range], 2, sum)
  sums = c("cp3-5% (Stability)", sums)
  tab_cp = rbind(tab_cp, sums)
  tab_cp[, -1] <- lapply(tab_cp[, -1], as.numeric)
  tab_cp[2,1] = "cp1% (Enrichment)"
  tab_cp[3,1] = "cp2% (Stress)"
  tab_cp = as.data.frame(tab_cp)
  tab_cp2 = as.data.frame(tab_cp2)
  rownames(tab_cp) = tab_cp[,1]
  rownames(tab_cp2) = tab_cp2[,1]
  tab_cp = tab_cp[,-1]
  tab_cp2 = tab_cp2[,-1]
  tab_cp = as.data.frame(t(tab_cp))
  tab_cp2 = as.data.frame(t(tab_cp2))
  tab_cp$SampleID = rownames(tab_cp)
  tab_cp2$SampleID = rownames(tab_cp2)
  result = merge(meta,tab_cp,by = "SampleID")
  result = merge(result, tab_cp2, by = "SampleID")
  result = tibble::as_tibble(result)
  .ter@result = result
  return(.ter)
}