#' An S4 class to store funguild2 results.
#' @slot result A data frame of funguild2 results.
methods::setClass("funguild2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "funguild2", function(object){
  cat("This is an beta object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' calc_funguild2
#' @description Calculate funguild2 between treatments.
#' @param data nemindex type data.
#' @param .group1 The group variable.
#' @param .group2 The group variable.
#' @return An funguild2 object.
#' @export
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