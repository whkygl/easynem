#' Calculation of energy flow in nematode communities (single factor)
#'
#' The \code{calc_ef()} function is used to calculate the energy flow of a nematode
#' community. For detailed calculation method, see \code{Wan et al. (2022)}: Step 1, the
#' fresh biomass of each nematode individuals was calculated based on the measurement
#' of body size or using publicly available data. Step 2, nematode metabolism (F) was
#' then calculated according to \code{Ferris (2010)} and \code{van den Hoogen et al. (2019)},
#' where Nt, Wt and mt are the number of individuals, the fresh weight and the cp class
#' of taxon t, respectively. Step 3, a five-node food web topology was constructed and
#' the feeding preferences of omnivores-carnivores on other trophic groups was assumed according
#' to community density. Step 4, the metabolism of each node was summed by all individual
#' metabolism of the respective trophic group. Step 5, we used assimilation efficiencies
#' (ea) of 0.25 for herbivores, 0.60 for bacterivores, 0.38 for fungivores and 0.5
#' for omnivores-carnivores according to \code{Barnes et al. (2014)} and \code{De Ruiter et al. (1993)}.
#' Step 6, energy fluxes between nodes was calculated as follows: \code{Fi = (F + L)/ea},
#' where L is the energy loss to higher trophic levels.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_ter <- nem |> nem_index() |> calc_ef(con_crop)
#' ```
#'
#' @usage calc_ef(data, .group)
#'
#' @param data An \code{\link{nemindex-class}} object.
#' @param .group The group variable.
#'
#' @return An \code{\link{ef-class}} object that stores the desired visualization results.
#' * \code{OF}, Energy flow metabolism of omnivorous predatory nematodes.
#' * \code{OM}, Fresh biomass (μg / 100g dry soil) of omnivorous predatory nematodes.
#' * \code{BF}, Energy flow metabolism of bacteria-feeding nematodes.
#' * \code{BM}, Fresh biomass (μg / 100g dry soil) of omnivorous predatory nematodes.
#' * \code{HF}, Energy flow metabolism of herbivorous nematodes.
#' * \code{HM}, Fresh biomass (μg / 100g dry soil) of herbivorous nematodes.
#' * \code{FF}, Energy flow metabolism of fungus-feeding nematodes.
#' * \code{FM}, Fresh biomass (μg / 100g dry soil) of fungus-feeding nematodes.
#' * \code{bp}, Feeding preference of predatory nematodes over bacteria-feeding nematodes.
#' * \code{hp}, Feeding preference of predatory nematodes over herbivorous nematodes.
#' * \code{fp}, Feeding preferences of predatory nematodes over fungivorous nematodes.
#' * \code{fbo}, Energy flow (μg C / 100g dry soil / day) between bacteria-feeding nematodes and omnivorous predatory nematodes.
#' * \code{fho}, Energy flow (μg C / 100g dry soil / day) between herbivorous nematodes and omnivorous predatory nematodes.
#' * \code{ffo}, Energy flow (μg C / 100g dry soil / day) between fungus-feeding nematodes and omnivorous predatory nematodes.
#' * \code{frb}, Energy flow (μg C / 100g dry soil / day) between basal resources and bacteria-feeding nematodes.
#' * \code{frh}, Energy flow (μg C / 100g dry soil / day) between basal resources and herbivorous nematodes.
#' * \code{frf}, Energy flow (μg C / 100g dry soil / day) between basal resources and fungivorous nematodes.
#' * \code{U}, Uniformity (U) of soil nematode energetic structure (unitless, mean ± standard error) was calculated as the
#' ratio of the mean of summed energy flux through each energy channel to the standard deviation of these mean values.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_nemindex}},
#' \code{\link{calc_funguild}}, \code{\link{calc_funguild2}}, \code{\link{calc_mf2}},
#' \code{\link{calc_mf}}, \code{\link{calc_ter2}}, \code{\link{calc_ter}},
#' \code{\link{calc_ef2}}.
#'
#' @references
#' * Wan, Bingbing, et al. "Organic amendments increase the flow uniformity of
#' energy across nematode food webs." Soil Biology and Biochemistry 170 (2022): 108695.
#' * Ferris, H., 2010. Form and function: metabolic footprints of nematodes in the soil
#' food web. European Journal of Soil Biology 46, 97–104.
#' * Van Den Hoogen, Johan, et al. "Soil nematode abundance and functional group
#' composition at a global scale." Nature 572.7768 (2019): 194-198.
#' * Barnes, A.D., Jochum, M., Mumme, S., Haneda, N.F., Farajallah, A., Widarto, T.H.,
#' Brose, U., 2014. Consequences of tropical land use for multitrophic biodiversity and
#' ecosystem functioning. Nature Communications 5, 1–7.
#' * De Ruiter, P.C., Van Veen, J.A., Moore, J.C., Brussaard, L., Hunt, H.W., 1993. Calculation
#' of nitrogen mineralization in soil food webs. Plant and Soil 157, 263–273.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |> calc_nemindex() |> calc_ef(Treatments)
#' nem_index
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
  alltab2 = dplyr::mutate(alltab2, U = rowMeans(dplyr::select(alltab2,13:18))/stats::sd(dplyr::c_across(13:18)))
  meta = meta[,c("SampleID", .group)]
  alltab2 = merge(meta, alltab2, by = "SampleID")
  alltab2 = tibble::as_tibble(alltab2)
  .ef@result = alltab2
  .ef@tab = data@tab
  .ef@tax = data@tax
  .ef@meta = merge(data@meta, alltab2[,-2], by = "SampleID")
  return(.ef)
}
