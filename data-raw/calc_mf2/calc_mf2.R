devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"), 
                tax = easynem_example("nemtax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- calc_nemindex(bac)
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
hehe2 <- calc_mf2(hehe, con_crop, season)
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object mf or other types data.
#' @param kei Adjust the proportion of the vertical axis of the graph.
#' @param ksi Adjust the proportion of the abscissa of the graph.
#' @param ... Other parameters for ggplot2 functions.
#' @return An ggplot object.
#' @rdname mf2
#' @name mf2
#' @aliases nem_plot,mf2-method
#' @import ggplot2
#' @export
setMethod("nem_plot", signature("mf2"), function(object, kei = 1, ksi = 1, ...){
  # object = hehe2
  # kei = 3
  # ksi = 3
  meta = object@result
  meta2 = stats::na.omit(meta)
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2])) |> 
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output$meanfufp = (output$meanefp * output$meansfp) / 2
  output$meanfufp_ = (output$meanfufp/sum(output$meanfufp))*100
  output$meanfufp_ = round(output$meanfufp_, 2)
  output$meanfufp_ = as.character(output$meanfufp_)
  output$meanfufp_ = paste0(output$meanfufp_,"%")
  output1 = output$meanfufp_
  names(output1) = output[[1]]
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[3])) |> 
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output$meanfufp = (output$meanefp * output$meansfp) / 2
  output$meanfufp_ = (output$meanfufp/sum(output$meanfufp))*100
  output$meanfufp_ = round(output$meanfufp_, 2)
  output$meanfufp_ = as.character(output$meanfufp_)
  output$meanfufp_ = paste0(output$meanfufp_,"%")
  output2 = output$meanfufp_
  names(output2) = output[[1]]
  output = meta2 |> dplyr::group_by(!!rlang::sym(names(meta2)[2]), !!rlang::sym(names(meta2)[3])) |> 
    dplyr::summarise(meanei = mean(EI), meansi = mean(SI), meanefp = mean(EnrichmentFootprint), meansfp = mean(StructureFootprint))
  output[[1]] <- sapply(output[[1]], function(x) {
    if (x %in% names(output1)) {
      paste0(x, "_", output1[x])
    } else {
      x
    }
  })
  output[[2]] <- sapply(output[[2]], function(x) {
    if (x %in% names(output2)) {
      paste0(x, "_", output2[x])
    } else {
      x
    }
  })
  output$meanei0.5p = output$meanei + 0.5 * output$meanefp/kei
  output$meanei0.5n = output$meanei - 0.5 * output$meanefp/kei
  output$meansi0.5p = output$meansi + 0.5 * output$meansfp/ksi
  output$meansi0.5n = output$meansi - 0.5 * output$meansfp/ksi
  output1 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi")]
  output2 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi0.5p")]
  colnames(output2) = colnames(output1)
  output3 = output[ , c(names(output)[1], names(output)[2], "meanei", "meansi0.5n")]
  colnames(output3) = colnames(output1)
  output4 = output[ , c(names(output)[1], names(output)[2], "meanei0.5p", "meansi")]
  colnames(output4) = colnames(output1)
  output5 = output[ , c(names(output)[1],names(output)[2], "meanei0.5n", "meansi")]
  colnames(output5) = colnames(output1)
  zoutput = rbind(output1, output5, output3, output4, output2)
  zoutputsub = zoutput |> dplyr::group_by(zoutput[[names(zoutput)[1]]], zoutput[[names(zoutput)[2]]]) |> dplyr::slice(-1)
  # return(zoutputsub)
  p <- ggplot2::ggplot(zoutput, ggplot2::aes(x = meansi, y = meanei, color = !!rlang::sym(names(zoutput)[1]), shape = !!rlang::sym(names(zoutput)[2]))) + ggplot2::geom_point(size = 3) + 
    ggplot2::xlab("Structure index") + ggplot2::ylab("Enrichment index") + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) + ggplot2::theme_test() +
    ggplot2::geom_hline(yintercept = c(0, 50, 100)) + ggplot2::geom_vline(xintercept = c(0, 50, 100)) + ggplot2::geom_polygon(data = zoutputsub, ggplot2::aes(x = meansi, y = meanei, fill = !!rlang::sym(names(zoutput)[1])), alpha = 0.2)
  p
})
hehe3 = nem_plot(hehe2,1000, 15)
hehe3
