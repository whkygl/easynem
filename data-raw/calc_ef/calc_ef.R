devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"), 
                tax = easynem_example("nemtax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- calc_nemindex(bac)
#' An S4 class to store energy flow results.
#' @slot result A data frame of energy flow results.
methods::setClass("ef",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "ef", function(object){
  cat("This is an ef object\n")
  cat("The energy flow results is:\n")
  print(object@result)
})
#' calc_ef
#' @description Calculate energy flow between treatments.
#' @param data nemindex type data.
#' @param .group The group variable.
#' @return An ef object.
#' @export
calc_ef <- function(data, .group){
  # data = hehe
  # .group = "con_crop"
  .ef = methods::new("ef")
  nemindex = data@result
  tab = data@tab
  tax = data@tax
  meta = data@meta
  .group = deparse(substitute(.group))
  alltab = merge(tab, tax, by = "OTUID")
  otab = alltab |> dplyr::filter(!!rlang::sym("feeding") %in% c(5,8))
  otab = otab[,c(names(tab),"cp_value", "GenavgMass")]
  otab2 = as.data.frame(colSums(otab[,-c(1, ncol(otab), ncol(otab)-1)]*(0.104*(otab$GenavgMass/otab$cp_value/12)+0.0159*(otab$GenavgMass^0.75))))
  names(otab2) = "OF"
  otab2$OF = signif(otab2$OF, 2)
  otab2$SampleID = rownames(otab2)
  otab3 = as.data.frame(colSums(otab[,-c(1, ncol(otab), ncol(otab)-1)]*otab$GenavgMass))
  names(otab3) = "OM"
  otab3$SampleID = rownames(otab3)
  otab2 = merge(otab2,otab3, by = "SampleID")
  btab = alltab |> dplyr::filter(!!rlang::sym("feeding") %in% c(3))
  btab = btab[,c(names(tab),"cp_value", "GenavgMass")]
  btab2 = as.data.frame(colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)]*(0.104*(btab$GenavgMass/btab$cp_value/12)+0.0159*(btab$GenavgMass^0.75))))
  names(btab2) = "BF"
  btab2$SampleID = rownames(btab2)
  btab2$BF = signif(btab2$BF, 2)
  btab3 = as.data.frame(colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)]*btab$GenavgMass))
  names(btab3) = "BM"
  btab3$SampleID = rownames(btab3)
  btab2 = merge(btab2,btab3, by = "SampleID")
  htab = alltab |> dplyr::filter(!!rlang::sym("feeding") %in% c(1))
  htab = htab[,c(names(tab),"cp_value", "GenavgMass")]
  htab2 = as.data.frame(colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)]*(0.104*(htab$GenavgMass/htab$cp_value/12)+0.0159*(htab$GenavgMass^0.75))))
  names(htab2) = "HF"
  htab2$SampleID = rownames(htab2)
  htab2$HF = signif(htab2$HF, 2)
  htab3 = as.data.frame(colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)]*htab$GenavgMass))
  names(htab3) = "HM"
  htab3$SampleID = rownames(htab3)
  htab2 = merge(htab2,htab3, by = "SampleID")
  ftab = alltab |> dplyr::filter(!!rlang::sym("feeding") %in% c(2))
  ftab = ftab[,c(names(tab),"cp_value", "GenavgMass")]
  ftab2 = as.data.frame(colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)]*(0.104*(ftab$GenavgMass/ftab$cp_value/12)+0.0159*(ftab$GenavgMass^0.75))))
  names(ftab2) = "FF"
  ftab2$SampleID = rownames(ftab2)
  ftab2$FF = signif(ftab2$FF, 2)
  ftab3 = as.data.frame(colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)]*ftab$GenavgMass))
  names(ftab3) = "FM"
  ftab3$SampleID = rownames(ftab3)
  ftab2 = merge(ftab2,ftab3, by = "SampleID")
  alltab2 = otab2 |> dplyr::full_join(btab2, by = "SampleID") |> dplyr::full_join(htab2, by = "SampleID") |> dplyr::full_join(ftab2, by = "SampleID")
  alltab2 = alltab2 |> dplyr::select(!!rlang::sym("SampleID"), dplyr::everything())
  bp = colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)])/(colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)]) + colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)]) + colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)]))
  bp = as.data.frame(bp)
  bp$SampleID = rownames(bp)
  hp = colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)])/(colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)]) + colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)]) + colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)]))
  hp = as.data.frame(hp)
  hp$SampleID = rownames(hp)
  fp = colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)])/(colSums(btab[,-c(1, ncol(btab), ncol(btab)-1)]) + colSums(htab[,-c(1, ncol(htab), ncol(htab)-1)]) + colSums(ftab[,-c(1, ncol(ftab), ncol(ftab)-1)]))
  fp = as.data.frame(fp)
  fp$SampleID = rownames(fp)
  alltab2 = alltab2 |> dplyr::full_join(bp, by = "SampleID") |> dplyr::full_join(hp, by = "SampleID") |> dplyr::full_join(fp, by = "SampleID")
  alltab2 = alltab2 |> dplyr::mutate(fbo = OF /0.5 * bp)
  alltab2 = alltab2 |> dplyr::mutate(fho = OF /0.5 * hp)
  alltab2 = alltab2 |> dplyr::mutate(ffo = OF /0.5 * fp)
  alltab2 = alltab2 |> dplyr::mutate(frb = (BF+(OF /0.5 * bp))/0.6)
  alltab2 = alltab2 |> dplyr::mutate(frh = (HF+(OF /0.5 * hp))/0.25)
  alltab2 = alltab2 |> dplyr::mutate(frf = (FF+(OF /0.5 * fp))/0.38)
  alltab2 = dplyr::mutate(alltab2, U = rowMeans(select(alltab2,13:18))/stats::sd(dplyr::c_across(13:18)))
  meta = meta[,c("SampleID", .group)]
  alltab2 = merge(meta, alltab2, by = "SampleID")
  alltab2 = tibble::as_tibble(alltab2)
  .ef@result = alltab2
  return(.ef)
}
hehe <- bac |> calc_nemindex() |> calc_ef(con_crop)
hehe
