#' Calculating the metabolic footprint of nematodes (single factor)
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
#' nem_fun <- nem |> calc_nemindex() |> calc_mf(con_crop)
#' ```
#'
#' @usage calc_mf(data, .group)
#'
#' @param data A \code{\link{nemindex-class}} object.
#' @param .group The group variable.
#'
#' @return A \code{\link{mf-class}} object that stores the desired
#' visualization results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf2}},
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
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |> calc_nemindex() |> calc_mf(Treatments)
#' nem_index
calc_mf <- function(data, .group){
  # data = hehe
  # .group = "con_crop"
  .mf = methods::new("mf")
  mf = data@result
  .group = deparse(substitute(.group))
  mf = mf[,c(names(mf)[1], .group,"EI", "SI", "EnrichmentFootprint", "StructureFootprint")]
  .mf@result = mf
  return(.mf)
}
