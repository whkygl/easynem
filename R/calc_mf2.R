#' Calculating the metabolic footprint of nematodes (two-factor)
#'
#' Metabolic footprints quantify the amplitude of Carbon utilisation by different
#' food web components. The point in the middle of a rhombus represents the
#' intersection of EI and SI and length of vertical and horizontal axes of the
#' rhombus corresponds to the footprints of enrichment and structure components
#' respectively.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_fun <- nem |> calc_nemindex() |> calc_mf2(con_crop, season)
#' ```
#'
#' @usage calc_mf2(data, .group1, .group2)
#'
#' @param data A \code{\link{nemindex-class}} object.
#' @param .group1 The group variable factor 1.
#' @param .group2 The group variable factor 2.
#'
#' @return A \code{\link{mf2-class}} object that stores the desired
#' visualization results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @references
#' * <https://shiny.wur.nl/ninja/>
#' * Ferris, Howard. "Form and function: metabolic footprints of nematodes in the
#' soil food web." European Journal of Soil Biology 46.2 (2010): 97-104.
#'
#' @export
#' @examples
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_index <- nem |> calc_nemindex() |> calc_mf2(con_crop, season)
#' nem_index
calc_mf2 <- function(data, .group1, .group2){
  # data = hehe
  # .group1 = "con_crop"
  # .group2 = "season"
  .mf = methods::new("mf2")
  mf = data@result
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  mf = mf[,c(names(mf)[1], .group1, .group2, "EI", "SI", "EnrichmentFootprint", "StructureFootprint")]
  .mf@result = mf
  return(.mf)
}
