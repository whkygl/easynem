#' Table of soil nematode abundance in kiwifruit orchards
#'
#' Abundance (individuals / 100 g dry soil) of nematodes functional guilds under
#' different cover crop diversity treatments.
#' The variables are as follows:
#' @usage nemtab
#' @format A tibble with 46 rows and 13 variables (The numbers after _ in the
#' columns represent the replicates of each treatment):
#' \describe{
#'   \item{OTUID}{Taxonomic ID of nematodes}
#'   \item{CK}{No cover crop}
#'   \item{C2}{Two cover crop species}
#'   \item{C4}{Four cover crop species}
#'   \item{C8}{Eight cover crop species}
#' }
#' @references This dataset referenced from "Li Q-m, Qi X-X, Zhang H-f, Zhang Y-j,
#' Liu H-m, Zhao J-n, Yang D and Wang H (2023) Responses of soil nematode abundance
#' and food web to cover crops in a kiwifruit orchard. Front. Plant Sci. 14:1173157.
#' doi: 10.3389/fpls.2023.1173157"
#' @examples
#' data(nemtab)
#' head(nemtab)
"nemtab"
