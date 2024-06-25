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
#' Alpha Class
#'
#' This class represents an extension of the \code{easynem} class.
#'
#' @slot result The calculation results of storage alpha diversity.
#' @export
methods::setClass(
  "alpha",
  contains = "easynem",
  methods::representation(result = "data.frame")
)
methods::setMethod(
  "show",
  "alpha",
  function(object) {
    cat("The alpha diversity of each treatment result is:\n")
    print(object@result)
  }
)
#' calc_alpha
#' @description Alpha diversity calculation for the tab.
#' @param data easynem type data.
#' @param ... Other parameters for calc_alpha.
#' @return An alpha object.
#' @export
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