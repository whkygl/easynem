devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"), 
                tax = easynem_example("nemtax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- calc_nemindex(bac)
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
hehe2 <- calc_funguild(hehe, con_crop)
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object funguild or other types data.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname funguild
#' @name funguild
#' @aliases nem_plot,funguild-method
#' @import ggplot2
#' @export
setMethod("nem_plot", signature("funguild"), function(object, ...){
  # object = hehe2
  meta = object@result
  meta2 = stats::na.omit(meta)
  p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym("SI"), y = !!rlang::sym("EI"), color = !!rlang::sym(names(meta2)[2]))) +
    ggplot2::geom_hline(yintercept = 50) +
    ggplot2::geom_vline(xintercept = 50) +
    ggplot2::geom_point() + ggplot2::theme_test() +
    ggplot2::ylab("Enrichment Index") +
    ggplot2::xlab("Structure Index")
  p
})
hehe3 = nem_plot(hehe2)
