#' Nematode food web analysis (two-factor)
#'
#' The \code{calc_funguild2()} is used for nematode food web analysis and generate
#' \code{\link{funguild2-class}}.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_fun <- nem |> calc_funguild2(con_crop, season)
#' ```
#'
#' @usage calc_funguild2(data, .group1, .group2)
#'
#' @param data A \code{\link{nemindex-class}} object.
#' @param .group1 The group variable factor 1.
#' @param .group2 The group variable factor 2.
#'
#' @return A \code{\link{funguild2-class}} object that stores the desired
#' visualization results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
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
#' nem <- read_nem(tab = easynem_example("nemtab1.csv"),
#'                 tax = easynem_example("nemtax1.csv"),
#'                 meta = easynem_example("nemmeta1.csv"))
#' nem_index <- nem |> calc_nemindex() |> calc_funguild2(con_crop, season)
#' nem_index
calc_funguild2 <- function(data, .group1, .group2){
  # data = hehe
  # .group1 = "con_crop"
  # .group2 = "season"
  .funguild = methods::new("funguild2")
  funguild = data@result
  .group1 = deparse(substitute(.group1))
  .group2 = deparse(substitute(.group2))
  funguild = funguild[,c(names(funguild)[1], .group1, .group2, "EI", "SI")]
  .funguild@result = funguild
  return(.funguild)
}
