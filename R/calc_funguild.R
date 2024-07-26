#' Nematode food web analysis (single factor)
#'
#' The \code{calc_funguild()} is used for nematode food web analysis and generate
#' \code{\link{funguild-class}}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_fun <- nem |> calc_funguild(con_crop)
#' ```
#'
#' @usage calc_funguild(data, .group)
#'
#' @param data A \code{\link{nemindex-class}} object.
#' @param .group The group variable.
#'
#' @return A \code{\link{funguild-class}} object that stores the desired
#' visualization results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @references
#' * <https://shiny.wur.nl/ninja/>
#' * Ferris, Howard, Tom Bongers, and Ron GM de Goede. "A framework for soil food
#' web diagnostics: extension of the nematode faunal analysis concept." Applied
#' soil ecology 18.1 (2001): 13-29.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |> calc_nemindex() |> calc_funguild(Treatments)
#' nem_index
calc_funguild <- function(data, .group){
  # data = hehe
  # .group = "con_crop"
  .funguild = methods::new("funguild")
  funguild = data@result
  .group = deparse(substitute(.group))
  funguild = funguild[,c(names(funguild)[1], .group,"EI", "SI")]
  .funguild@result = funguild
  return(.funguild)
}
