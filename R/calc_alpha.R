methods::setClass("easynem",
    slots = list(
        tab = "data.frame",
        tax = "data.frame",
        meta = "data.frame"
    ))
methods::setMethod("show", "easynem", function(object){
  cat("This is an easynem object\n")
  cat("The otutab is:\n")
  print(object@tab)
  cat("The taxonomy is:\n")
  print(object@tax)
  cat("The metadata is:\n")
  print(object@meta)
})
#' Alpha diversity analysis, generating alpha-class
#'
#' The \code{calc_alpha()} is used to perform alpha diversity analysis and create
#' \code{\link{alpha-class}}. This function can be used to calculate various alpha
#' diversity indices such as \code{Chao1}, \code{ACE}, \code{Shannon}, \code{Simpson},
#' etc.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_alpha <- nem |> calc_alpha()
#' ```
#'
#' @usage calc_alpha(data, ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param ... Other parameters for \code{\link[vegan]{diversity}}, \code{\link[vegan]{simpson.unb}},
#' \code{\link[vegan]{fisher.alpha}}, \code{\link[vegan]{specnumber}}, and \code{\link[vegan]{estimateR}}.
#'
#' @return A \code{\link{alpha-class}} for storing alpha diversity analysis results.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_nemindex}}, \code{\link{calc_funguild}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_alpha <- nem |> calc_alpha()
#' show(nem_alpha)
calc_alpha <- function(data, ...){
  # data = bac
  alpha = methods::new("alpha")
  alpha@meta = data@meta
  alpha@tax = data@tax
  alpha@tab = data@tab
  results = data@meta[, sapply(data@meta, is.character)]
  tab = as.data.frame(alpha@tab)
  rownames(tab) = tab[,1]
  tab = tab[,-1]
  tab_t = t(tab)
  Shannon = vegan::diversity(tab_t, "shannon", ...)
  Simpson = vegan::diversity(tab_t, "simpson", ...)
  Invsimpson = vegan::diversity(tab_t, "invsimpson", ...)
  UnbiasedSimpson = vegan::simpson.unb(tab_t, ...)
  FihserAlpha = vegan::fisher.alpha(tab_t, ...)
  SpeciesNumber = vegan::specnumber(tab_t, ...)
  PielouEvenness = Shannon/log(SpeciesNumber)
  GenericRichness = (SpeciesNumber-1)/log(rowSums(tab_t))
  result1 = vegan::estimateR(tab_t, ...)
  result1 = t(result1)
  result1 = result1[,c(2,4)]
  colnames(result1) = c("Chao1", "ACE")
  result1 = as.data.frame(result1)
  result1$Shannon = Shannon
  result1$Simpson = Simpson
  result1$Invsimpson = Invsimpson
  result1$UnbiasedSimpson = UnbiasedSimpson
  result1$FihserAlpha = FihserAlpha
  result1$SpeciesNumber = SpeciesNumber
  result1$PielouEvenness = PielouEvenness
  result1$GenericRichness = GenericRichness
  result1$SampleID = rownames(result1)
  meta = tibble::as_tibble(merge(alpha@meta, result1, by = "SampleID"))
  alpha@meta = meta
  results = merge(results, result1, by = "SampleID")
  results = tibble::as_tibble(results)
  alpha@result = results
  return(alpha)
}
