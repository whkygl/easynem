#' An S4 class to store funguild results.
#' @slot result A data frame of funguild results.
methods::setClass("funguild",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "funguild", function(object){
  cat("This is an beta object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
#' calc_funguild
#' @description Calculate calc_funguild between treatments.
#' @param data nemindex type data.
#' @param .group The group variable.
#' @return An funguild object.
#' @export
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