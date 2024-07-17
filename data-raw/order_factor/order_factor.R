devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"),
                tax = easynem_example("nemtax.csv"),
                meta = easynem_example("meta.csv"))
hehe <- bac |>
#' order_factor
#'
#' Functions for reordering factors.
#' @param object easynem or other types data.
#' @param ... Other parameters.
#' @export
setGeneric("order_factor", function(object,...){
  standardGeneric("order_factor")
})
#' order_factor
#' @description Functions for reordering factors.
#' @param object beta types data.
#' @param .group Vector of factor orders.
#' @return An beta object.
#' @rdname beta
#' @name beta
#' @aliases order_factor,beta-method
#' @export
setMethod("order_factor", signature("beta"), function(object, .group){
  # object = hehe

})
