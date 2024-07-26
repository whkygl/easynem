#' Trivariate analysis of nematode feeding or cp value (single factor)
#'
#' The \code{calc_ter()} function is used to perform ternary analysis on nematode
#' feeding (Relative biomass of bacteria-feeding nematodes, fungi-feeding nematodes,
#' and herbivorous nematodes) or cp values (Relative abundance of cp1 nematodes,
#' cp2 nematodes, and cp3-5 nematodes).
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_ter <- nem |> calc_ter(con_crop)
#' ```
#' @usage calc_ter(data, .group)
#'
#' @param data An \code{\link{easynem-class}} object.
#' @param .group The group variable.
#'
#' @return A \code{\link{ter-class}} object that stores the desired
#' visualization results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf2}},
#' \code{\link{calc_mf}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @references
#' * <https://shiny.wur.nl/ninja/>
#' * Goede, RGM de, T. Bongers, and C. H. Ettema. "Graphical presentation and
#' interpretation of nematode community structure: cp triangles." (1993): 743-750.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |> calc_ter(Treatments)
#' nem_index
calc_ter <- function(data, .group){
  # data = bac
  # .group = "con_crop"
  .group = deparse(substitute(.group))
  .ter = methods::new("ter")
  cp_tax = data@tax[,c("OTUID", "feeding", "cp_value")]
  cp_tax2 = data@tax[,c("OTUID", "feeding", "GenavgMass")]
  cp_tax$cp_value[cp_tax$feeding == 1] = 0
  cp_tax = cp_tax[,-2]
  tab = data@tab
  meta = data@meta[,c("SampleID", .group)]
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
