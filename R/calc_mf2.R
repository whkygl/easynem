#' An S4 class to store Metabolic footprints results.
#' @slot result A data frame of Metabolic footprints results.
methods::setClass("mf2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "mf2", function(object){
  cat("This is an beta object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' calc_mf2
#' @description Calculate Metabolic footprints between treatments.
#' @param data nemindex type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @return An mf2 object.
#' @export
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