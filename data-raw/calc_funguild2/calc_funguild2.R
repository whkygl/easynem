devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"), 
                tax = easynem_example("nemtax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- calc_nemindex(bac)
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
hehe2 <- calc_funguild2(hehe, con_crop, season)
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object funguild2 or other types data.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname funguild2
#' @name funguild2
#' @aliases nem_plot,funguild2-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("funguild2"), function(object, ...){
  # object = hehe2
  meta = object@result
  meta2 = stats::na.omit(meta)
  p = ggplot2::ggplot(meta2, ggplot2::aes(x = !!rlang::sym("SI"), y = !!rlang::sym("EI"), color = !!rlang::sym(names(meta2)[2]), shape = !!rlang::sym(names(meta2)[3]))) +
    ggplot2::geom_hline(yintercept = 50) +
    ggplot2::geom_vline(xintercept = 50) +
    ggplot2::geom_point() + ggplot2::theme_test() +
    ggplot2::ylab("Enrichment Index") +
    ggplot2::xlab("Structure Index")
  p
})
hehe3 = nem_plot(hehe2)
