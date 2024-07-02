#' An S4 class to store Metabolic footprints results.
#' @slot result A data frame of Metabolic footprints results.
methods::setClass("mf",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "mf", function(object){
  cat("This is an beta object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' calc_mf
#' @description Calculate Metabolic footprints between treatments.
#' @param data nemindex type data.
#' @param .group The group variable.
#' @return An mf object.
#' @export
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