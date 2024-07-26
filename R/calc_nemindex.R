methods::setClass("easynem",
                  slots = list(
                    tab = "data.frame",
                    tax = "data.frame",
                    meta = "data.frame"
                  ))
methods::setMethod("show", "easynem", function(object){
  cat("This is an easynem object\n")
  cat("The otutab is:\n")
  print(object@tab)
  cat("The taxonomy is:\n")
  print(object@tax)
  cat("The metadata is:\n")
  print(object@meta)
})
#' Calculate multiple nematode ecological indices and generate nemindex class
#'
#' The \code{calc_nemindex()} is used to Calculate multiple nematode ecological
#' indices and generate \code{\link{nemindex-class}}. The ecological indexes that
#' can be calculated by this function include \code{MI}, \code{sigMI}, \code{sigMI25},
#' \code{MI25}, \code{PPI}, \code{WI}, \code{NCR}, \code{CI}, \code{BI}, \code{SI},
#' \code{EI}, etc.
#'
#' To facilitate code interpretation, it is recommended to use the pipe symbol
#' [`|>`] to connect functions:
#'
#' ```
#' nem_index <- nem |> calc_nemindex()
#' ```
#'
#' @usage calc_nemindex(data)
#'
#' @param data An \code{\link{easynem-class}} data.
#'
#' @return
#' A \code{\link{nemindex-class}} for storing nematode ecological indices
#' analysis results.
#' * \code{MI}, Maturity Index. Indicates environmental disturbance resulting from
#' perturbations (range, 1-5). Low values (<2) indicate an early (primary or secondary)
#' successional stage or a temporary level of increased nutrient availability.
#' Values close to 2 indicate a high level of disturbance with low soil food web
#' structure, while intermediate values (2.5–3) indicate some soil food web maturity.
#' High values (>3) indicate a well-structured and complex soil food web likely with
#' connectivity and energy flow between trophic levels.
#' * \code{sigMI}, Sigma Maturity Index (∑MI). Indicates environmental disturbance
#' resulting from perturbations in non-agricultural soils (range, 1-5). Low values (<2)
#' indicate a high level of nutrient availability and minimal plant-parasitic pressure,
#' while values close to 2 indicate a high level of disturbance with low soil food web
#' structure. Intermediate values (2.5–3) indicate some soil food web maturity.
#' High values (>3), in turn, indicate a well-structured and complex soil food web
#' likely with connectivity and energy flow between trophic levels, which might include
#' larger plant-parasitic nematodes. This index is less sensitive to enrichment in
#' agricultural soils.
#' * \code{sigMI25}, Sigma Maturity Index 2-5 (∑MI25). computes the MI for all
#' nematodes in the c-p2-5 range (Neher & Campbell, 1996). The index recognizes
#' that the higher c-p value plant-feeding species also provide information of
#' environmental stress but bears some of the burden of the ΣMI in situations of
#' nutrient enrichment.
#' * \code{MI25}, Maturity Index 2–5. Indicates Environmental disturbance resulting
#' from perturbations unrelated to nutrient enrichment in agricultural fields (range, 2-5).
#' Low values (close to 2) indicate substantial disturbance resulting from perturbations
#' unrelated to nutrient enrichment. High values (>3) indicate greater maturity with
#' minimal or no effect resulting from perturbations.
#' * \code{PPI}, Plant-Parasitic Index. Indicates Assemblage composition of plant-parasitic
#' nematodes (range, 2-5). Low values (close to 2) indicate plant-parasitic nematode
#' assemblages dominated by small and medium-sized ectoparasites that feed on single
#' plant cells. Higher values indicate assemblages dominated by medium and large (semi-)
#' endoparasitic (e.g., Meloidogyne and Heterodera spp.) or ectoparasitic virus transmitting
#' nematodes (e.g., Xiphinema and Longidorus spp.).
#' * \code{PPI_MI}, PPI/MI. The PPI/MI ratio is lower under nutrient poor conditions
#' than under nutrient rich conditions. It is a sensitive indicator of enrichment
#' in agroecosystems (Bongers & Korthals, 1995; Bongers et al., 1997).
#' * \code{WI}, Wasilewska Index. Wasilewska Index is calculated by dividing the
#' sum of bacteria-feeding nematodes and fungi-feeding nematodes by the number of
#' herbivorous nematodes. This index is used to indicate the impact of nematode communities
#' on crop production. The smaller the index, the greater the negative impact of nematode
#' communities on crop production.
#' * \code{NCR}, Nematode Channel Ratio. The Nematode Channel Ratio (NCR) is a parameter
#' used in soil ecology to assess the balance between bacterial and fungal energy
#' channels in the soil food web. This ratio is calculated by comparing the abundance
#' of bacterial-feeding nematodes to fungal-feeding nematodes. High NCR: Indicates
#' a bacterial-dominated energy channel. This is often found in soils with frequent
#' disturbance or high inputs of easily decomposable organic matter. Low NCR: Indicates
#' a fungal-dominated energy channel. This is commonly found in more stable, less
#' disturbed soils, such as forests or natural grasslands, where organic matter
#' decomposition is slower and more complex.
#' * \code{CI}, Channel Index. Indicates predominant decomposition pathway of organic matter (range, 0-100).
#' Lower values (<50) indicate increasing decomposition dominance by bacteria, while
#' higher values (>50) indicate increasing decomposition dominance by fungi. Bacterial
#' dominance indicates the presence of rapidly decomposed organic matter, while fungal
#' dominated decomposition indicates the slow breakdown of more complex organic matter.
#' The focus on opportunistic bacterial and fungal feeders makes this a highly responsive
#' index, which can be used to detect alternating decomposition pathways over time.
#' * \code{EI}, Enrichment Index. Indicates food availability and nutrient enrichment
#' (range, 0-100). Low (0–30), intermediate (30–60), and high (60–100) values indicate
#' equivalent levels of food availability (e.g., labile organic carbon) and nutrient enrichment.
#' * \code{SI}, Structure Index. Indicates Soil food web structure and complexity,
#' as well as disturbance due to environmental (e.g., salinity and drought) or
#' anthropogenic (e.g. tillage, mining, and chemical pollution) causalities (range, 0-100).
#' Low (0–30), intermediate (30–60), and high (60–100) values indicate equivalent levels
#' of soil food web complexity. Lower values are indicative of perturbed soil food webs,
#' while higher values indicate a structured soil food web.
#' * \code{BI}, Basal Index. Indicates food web structure and complexity (range, 0-100).
#' Low (0–30), intermediate (30–60), and high (60–100) values indicate equivalent
#' levels of soil perturbation. Therefore, higher values (>50) are indicative of
#' a depleted and damaged soil food web.
#' * \code{TotalBiomass}, Total biomass of nematode community.
#' * \code{MetabolicFootprint}, Metabolic Footprints. Indicates magnitude of
#' ecosystem functions and services fulfilled by nematode community (range, 0-infinite).
#' Higher metabolic footprint values are indicative of greater carbon channelling
#' and therefore an increased contribution to the fulfilment of soil ecosystem
#' functions and services. This can be considered per trophic group (e.g. bacterivore
#' footprint), or per component of the nematode community that indicate enrichment
#' (enrichment footprint) and structure (structure footprint).
#' * \code{EnrichmentFootprint}, Enrichment Footprint.
#' * \code{StructureFootprint}, Structure Footprint.
#' * \code{HerbivoreFootprint}, Herbivore Footprint.
#' * \code{FungivoreFootprint}, Fungivore Footprint
#' * \code{BacterivoreFootprint}, Bacterivore Footprint.
#' * \code{PrOmFootprint}, Metabolic footprint of an omnivorous predatory nematode.
#' * \code{Numbers}, Number of nematodes.
#' * \code{CAssimilated}, Carbon assimilated by nematodes.
#' * \code{CRespired}, Carbon consumed by nematode respiration.
#'
#' @seealso
#' Other functions in this R package for data calculations:
#' \code{\link{calc_beta2}}, \code{\link{calc_compare}}, \code{\link{calc_compare2}},
#' \code{\link{calc_beta}}, \code{\link{calc_alpha}}, \code{\link{calc_funguild}},
#' \code{\link{calc_funguild2}}, \code{\link{calc_mf}}, \code{\link{calc_mf2}},
#' \code{\link{calc_ter}}, \code{\link{calc_ter2}}, \code{\link{calc_ef}},
#' \code{\link{calc_ef2}}.
#'
#' @references
#' * <https://shiny.wur.nl/ninja/>
#' * <http://nemaplex.ucdavis.edu/Ecology/Indices_of_ecosystem_condition.html>
#' * Du Preez G, Daneel M, De Goede R, et al. Nematode-based indices in soil ecology:
#' Application, utility, and future directions. Soil Biology and Biochemistry,
#' 2022, 169: 108640.
#' * Bongers T. The maturity index: an ecological measure of environmental
#' disturbance based on nematode species composition. Oecologia, 1990, 83: 14-19.
#' * Bongers T, Goede R G N, Korthals G W, et al. Proposed changes of cp classification
#' for nematodes. 1995.
#' * Ferris, H. O. W. A. R. D., and Tom Bongers. "Indices developed specifically
#' for analysis of nematode assemblages." Nematodes as environmental indicators.
#' Wallingford UK: CABI, 2009. 124-145.
#' * Goede, RGM de, T. Bongers, and C. H. Ettema. "Graphical presentation and
#' interpretation of nematode community structure: cp triangles." (1993): 743-750.
#' * Ferris, Howard, Tom Bongers, and Ron GM de Goede. "A framework for soil food
#' web diagnostics: extension of the nematode faunal analysis concept." Applied
#' soil ecology 18.1 (2001): 13-29.
#' * Ferris, Howard. "Form and function: metabolic footprints of nematodes in the
#' soil food web." European Journal of Soil Biology 46.2 (2010): 97-104.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_index <- nem |> calc_nemindex()
#' show(nem_index)
calc_nemindex <- function(data){
  index = methods::new("nemindex")
  index@meta = data@meta
  index@tax = data@tax
  index@tab = data@tab
  result1 = index@tax
  results = data@meta[, sapply(data@meta, is.character)]
  otutab = index@tab
  columns_to_select <- c("OTUID", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  taxonomy = index@tax |> dplyr::select(dplyr::any_of(columns_to_select))
  namenemdbgenus = c("OTUID", setdiff(colnames(index@tax), colnames(taxonomy)))
  nemdbgenus = index@tax[,namenemdbgenus]
  nemdbgenus$FeedingGroup <- paste0("F", nemdbgenus$feeding)
  names(nemdbgenus)[names(nemdbgenus) == "cp_value"] <- "CP"
  nemdbgenus$Cp <- paste0("Cp", nemdbgenus$CP)
  nemdbgenus <- nemdbgenus |>
    dplyr::mutate(Feeding = dplyr::case_when(
      feeding == 1 ~ "h",
      feeding == 2 ~ "f",
      feeding == 3 ~ "b",
      feeding == 4 ~ "s",
      feeding == 5 ~ "p",
      feeding == 6 ~ "e",
      feeding == 7 ~ "d",
      feeding == 8 ~ "o",
      TRUE ~ NA_character_
    ))
  nemdbgenus$FunctionalGuild <- paste0(nemdbgenus$Feeding, nemdbgenus$CP)
  nemdbgenus <- nemdbgenus |>
    dplyr::mutate(Feeding = dplyr::case_when(
      feeding == 1 ~ "PlantFeeders",
      feeding == 2 ~ "FungusFeeders",
      feeding == 3 ~ "BacteriaFeeders",
      feeding == 4 ~ "SubstrateIngestion",
      feeding == 5 ~ "Predators",
      feeding == 6 ~ "EucaryoteFeeders",
      feeding == 7 ~ "AnimalParasites",
      feeding == 8 ~ "Omnivores",
      TRUE ~ NA_character_
    ))
  index1 <- function(otutab,taxonomy){
    alltab = merge(otutab, taxonomy, by = "OTUID", all.y = TRUE) |>
       merge(nemdbgenus, by = "OTUID", all = FALSE)
    alltab$GenavgCRs = 0.273 * (alltab$GenavgMass ^ 0.75)
    alltab$GenavgCPr = 0.1 * alltab$GenavgMass / alltab$CP
    alltab$GenavgMFP = alltab$GenavgCRs + alltab$GenavgCPr
    alltab = alltab |> dplyr::mutate(GenavgEFP = ifelse(GenavgEFP != 0, GenavgMFP, GenavgEFP),
                                     GenavgSFP = ifelse(GenavgSFP != 0, GenavgMFP, GenavgSFP),
                                     GenavgHFP = ifelse(GenavgHFP != 0, GenavgMFP, GenavgHFP),
                                     GenavgFFP = ifelse(GenavgFFP != 0, GenavgMFP, GenavgFFP),
                                     GenavgBFP = ifelse(GenavgBFP != 0, GenavgMFP, GenavgBFP),
                                     GenavgPFP = ifelse(GenavgPFP != 0, GenavgMFP, GenavgPFP))
    tballtab = alltab[ ,c("OTUID",colnames(otutab)[-1])]
    cpralltab = tballtab
    cpsalltab = tballtab
    mfalltab = tballtab
    emfalltab = tballtab
    smfalltab = tballtab
    fmfalltab = tballtab
    h1mfalltab = tballtab
    f1mfalltab = tballtab
    b1mfalltab = tballtab
    p1mfalltab = tballtab
    Numbers = tballtab
    tballtab[ ,-1] = tballtab[ ,-1] * alltab$GenavgMass
    cpralltab[ ,-1] = cpralltab[ ,-1] * alltab$GenavgCPr
    cpsalltab[ ,-1] = cpsalltab[ ,-1] * alltab$GenavgCRs
    mfalltab[ ,-1] = mfalltab[ ,-1] * alltab$GenavgMFP
    emfalltab[ ,-1] = emfalltab[ ,-1] * alltab$GenavgEFP
    smfalltab[ ,-1] = smfalltab[ ,-1] * alltab$GenavgSFP
    h1mfalltab[ ,-1] = h1mfalltab[ ,-1] * alltab$GenavgHFP
    f1mfalltab[ ,-1] = f1mfalltab[ ,-1] * alltab$GenavgFFP
    b1mfalltab[ ,-1] = b1mfalltab[ ,-1] * alltab$GenavgBFP
    p1mfalltab[ ,-1] = p1mfalltab[ ,-1] * alltab$GenavgPFP
    Numbers = colSums(Numbers[ ,-1])
    tballtab = stats::na.omit(tballtab)
    bfpoalltab = alltab[grepl("f|b|p|o|e", alltab$FunctionalGuild), ]
    bfpoalltab = bfpoalltab[!grepl("ap", alltab$FunctionalGuild), ]
    fbhalltab = alltab[grepl("f|b|h", alltab$FunctionalGuild), ]
    alltab = alltab |> dplyr::filter(!is.na(CP))
    sigalltab = alltab
    halltab = alltab[grepl("h", alltab$FunctionalGuild), ]
    alltab = alltab[grepl("f|b|s|p|e|o", alltab$FunctionalGuild), ]
    alltab = alltab[!grepl("ap", alltab$FunctionalGuild), ]
    alltab = alltab[, c(c("OTUID",colnames(otutab)[-1]), "Cp")]
    halltab = halltab[, c(c("OTUID",colnames(otutab)[-1]), "Cp")]
    sigalltab = sigalltab[, c(c("OTUID",colnames(otutab)[-1]), "Cp")]
    fbhalltab = fbhalltab[ , c(c("OTUID",colnames(otutab)[-1]), "Feeding")]
    bfpoalltab = bfpoalltab[ , c(c("OTUID",colnames(otutab)[-1]), "FunctionalGuild")]
    alltab[1] = alltab$Cp
    halltab[1] = halltab$Cp
    sigalltab[1] = sigalltab$Cp
    fbhalltab[1] = fbhalltab$Feeding
    bfpoalltab[1] = bfpoalltab$FunctionalGuild
    alltab = subset(alltab, select = -Cp)
    halltab = subset(halltab, select = -Cp)
    sigalltab = subset(sigalltab, select = -Cp)
    fbhalltab = subset(fbhalltab, select = -Feeding)
    bfpoalltab = subset(bfpoalltab, select = -FunctionalGuild)
    colnames(alltab)[1] = "Cp"
    colnames(halltab)[1] = "Cp"
    colnames(sigalltab)[1] = "Cp"
    colnames(fbhalltab)[1] = "Feeding"
    colnames(bfpoalltab)[1] = "FunctionalGuild"
    alltab25 = alltab[alltab$Cp != "Cp1", ]
    sigalltab25 = sigalltab[sigalltab$Cp != "Cp1", ]
    alltab = alltab |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    halltab = halltab |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    sigalltab = sigalltab |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    sigalltab25 = sigalltab25 |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    alltab25 = alltab25 |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    fbhalltab = fbhalltab |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    bfpoalltab = bfpoalltab |> dplyr::group_by_at(1) |> dplyr::summarise_all(sum)
    alltab = alltab |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~.x/sum(.x)))
    halltab = halltab |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~.x/sum(.x)))
    sigalltab = sigalltab |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~.x/sum(.x)))
    sigalltab25 = sigalltab25 |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~.x/sum(.x)))
    alltab25 = alltab25 |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~.x/sum(.x)))
    Cp = gsub("[^0-9]", "", alltab[,1])
    hCp = gsub("[^0-9]", "", halltab[,1])
    sigCp = gsub("[^0-9]", "", sigalltab[,1])
    sigCp25 = gsub("[^0-9]", "", sigalltab25[,1])
    Cp25 = gsub("[^0-9]", "", alltab25[,1])
    Cp = as.numeric(strsplit(Cp, "")[[1]])
    hCp = as.numeric(strsplit(hCp, "")[[1]])
    sigCp = as.numeric(strsplit(sigCp, "")[[1]])
    sigCp25 = as.numeric(strsplit(sigCp25, "")[[1]])
    Cp25 = as.numeric(strsplit(Cp25, "")[[1]])
    alltab[,-1] = alltab[,-1] * Cp
    halltab[,-1] = halltab[,-1] * hCp
    sigalltab[,-1] = sigalltab[,-1] * sigCp
    sigalltab25[,-1] = sigalltab25[,-1] * sigCp25
    alltab25[,-1] = alltab25[,-1] * Cp25
    MI = colSums(alltab[ ,-1])
    PPI = colSums(halltab[ ,-1])
    sigMI = colSums(sigalltab[ ,-1])
    sigMI25 = colSums(sigalltab25[ ,-1])
    MI25 = colSums(alltab25[ ,-1])
    tballtab = tballtab[stats::complete.cases(tballtab),]
    TotalBiomass = colSums(tballtab[ ,-1])
    mfalltab = mfalltab[stats::complete.cases(mfalltab),]
    MetabolicFootprint = colSums(mfalltab[ ,-1])
    emfalltab = emfalltab[stats::complete.cases(emfalltab),]
    EnrichmentFootprint = colSums(emfalltab[ ,-1])
    smfalltab = smfalltab[stats::complete.cases(smfalltab),]
    StructureFootprint = colSums(smfalltab[ ,-1])
    h1mfalltab = h1mfalltab[stats::complete.cases(h1mfalltab),]
    HerbivoreFootprint = colSums(h1mfalltab[ ,-1])
    f1mfalltab = f1mfalltab[stats::complete.cases(f1mfalltab),]
    FungivoreFootprint = colSums(f1mfalltab[ ,-1])
    b1mfalltab = b1mfalltab[stats::complete.cases(b1mfalltab),]
    BacterivoreFootprint = colSums(b1mfalltab[ ,-1])
    p1mfalltab = p1mfalltab[stats::complete.cases(p1mfalltab),]
    PrOmFootprint = colSums(p1mfalltab[ ,-1])
    cpralltab = cpralltab[stats::complete.cases(cpralltab),]
    CAssimilated = colSums(cpralltab[ ,-1])
    cpsalltab = cpsalltab[stats::complete.cases(cpsalltab),]
    CRespired = colSums(cpsalltab[ ,-1])
    TotalBiomass = as.data.frame(TotalBiomass)
    MetabolicFootprint = as.data.frame(MetabolicFootprint)
    EnrichmentFootprint = as.data.frame(EnrichmentFootprint)
    StructureFootprint = as.data.frame(StructureFootprint)
    HerbivoreFootprint = as.data.frame(HerbivoreFootprint)
    FungivoreFootprint = as.data.frame(FungivoreFootprint)
    BacterivoreFootprint = as.data.frame(BacterivoreFootprint)
    PrOmFootprint = as.data.frame(PrOmFootprint)
    Numbers = as.data.frame(Numbers)
    CAssimilated = as.data.frame(CAssimilated)
    CRespired = as.data.frame(CRespired)
    MI = as.data.frame(MI)
    PPI = as.data.frame(PPI)
    sigMI = as.data.frame(sigMI)
    sigMI25 = as.data.frame(sigMI25)
    MI25 = as.data.frame(MI25)
    TotalBiomass = tibble::rownames_to_column(TotalBiomass)
    MetabolicFootprint = tibble::rownames_to_column(MetabolicFootprint)
    EnrichmentFootprint = tibble::rownames_to_column(EnrichmentFootprint)
    StructureFootprint = tibble::rownames_to_column(StructureFootprint)
    HerbivoreFootprint = tibble::rownames_to_column(HerbivoreFootprint)
    FungivoreFootprint = tibble::rownames_to_column(FungivoreFootprint)
    BacterivoreFootprint = tibble::rownames_to_column(BacterivoreFootprint)
    PrOmFootprint = tibble::rownames_to_column(PrOmFootprint)
    Numbers = tibble::rownames_to_column(Numbers)
    CAssimilated = tibble::rownames_to_column(CAssimilated)
    CRespired = tibble::rownames_to_column(CRespired)
    MI = tibble::rownames_to_column(MI)
    PPI = tibble::rownames_to_column(PPI)
    sigMI = tibble::rownames_to_column(sigMI)
    sigMI25 = tibble::rownames_to_column(sigMI25)
    MI25 = tibble::rownames_to_column(MI25)
    output <- Reduce(function(x, y) merge(x, y, by = "rowname", all = TRUE), list(MI,sigMI,sigMI25,MI25,PPI))
    output = dplyr::mutate(output, PPI_MI = PPI/MI)
    fbhalltab = as.data.frame(fbhalltab)
    bfpoalltab = as.data.frame(bfpoalltab)
    bfpoalltab = stats::na.omit(bfpoalltab)
    rownames(fbhalltab) = fbhalltab[ ,1]
    rownames(bfpoalltab) = bfpoalltab[ ,1]
    fbhalltab = fbhalltab[ ,-1]
    bfpoalltab = bfpoalltab[ ,-1]
    rnamefill0 = c("b1", "b2", "b3", "b4", "b5", "f2", "f3", "f4", "f5", "o3", "o4", "o5", "p2", "p3", "p4", "p5", "e2", "e3", "e4", "e5")[!c("b1", "b2", "b3", "b4", "b5", "f2", "f3", "f4", "f5", "o3", "o4", "o5", "p2", "p3", "p4", "p5", "e2", "e3", "e4", "e5") %in% rownames(bfpoalltab)]
    bfpoalltab2 = data.frame(matrix(0, nrow = length(rnamefill0), ncol = ncol(bfpoalltab)))
    rownames(bfpoalltab2) = rnamefill0
    colnames(bfpoalltab2) = colnames(bfpoalltab)
    bfpoalltab = rbind(bfpoalltab, bfpoalltab2)
    WI = (fbhalltab["BacteriaFeeders", ] + fbhalltab["FungusFeeders", ]) / fbhalltab["PlantFeeders", ]
    rownames(WI) = "WI"
    WI = as.data.frame(t(WI)) |> tibble::rownames_to_column()
    NCR = fbhalltab["BacteriaFeeders", ] / (fbhalltab["BacteriaFeeders", ] + fbhalltab["FungusFeeders", ])
    rownames(NCR) = "NCR"
    NCR = as.data.frame(t(NCR)) |> tibble::rownames_to_column()
    output = merge(output, WI, by = "rowname")
    output = merge(output, NCR, by = "rowname")
    CI = 100 * (0.8 * bfpoalltab["f2", ] / (3.2 * bfpoalltab["b1", ] + 0.8 * bfpoalltab["f2", ]))
    b = 0.8 * (bfpoalltab["b2", ] + bfpoalltab["f2", ])
    e = 3.2 * bfpoalltab["b1", ] + 0.8 * bfpoalltab["f2", ]
    s = 1.8 * bfpoalltab["b3", ] + 3.2 * bfpoalltab["b4", ] + 5 * bfpoalltab["b5", ] + 1.8 * bfpoalltab["f3", ] + 3.2 * bfpoalltab["f4", ] + 5 * bfpoalltab["f5", ] + 0.8 * bfpoalltab["p2", ] + 0.8 * bfpoalltab["e2", ] + 3.2 * bfpoalltab["o4", ] + 5 * bfpoalltab["o5", ] + 1.8 * bfpoalltab["p3", ] + 3.2 * bfpoalltab["p4", ] + 5 * bfpoalltab["p5", ] + 1.8 * bfpoalltab["e3", ] + 3.2 * bfpoalltab["e4", ] + 5 * bfpoalltab["e5", ]
    EI = 100 * e / (e + b)
    SI = 100 * s / (s + b)
    BI = 100 * b / (b + e + s)
    rownames(CI) = "CI"
    CI = as.data.frame(t(CI)) |> tibble::rownames_to_column()
    output = merge(output, CI, by = "rowname")
    rownames(EI) = "EI"
    EI = as.data.frame(t(EI)) |> tibble::rownames_to_column()
    output = merge(output, EI, by = "rowname")
    rownames(SI) = "SI"
    SI = as.data.frame(t(SI)) |> tibble::rownames_to_column()
    output = merge(output, SI, by = "rowname")
    rownames(BI) = "BI"
    BI = as.data.frame(t(BI)) |> tibble::rownames_to_column()
    output = merge(output, BI, by = "rowname")
    output = merge(output, TotalBiomass, by = "rowname")
    output = merge(output, MetabolicFootprint, by = "rowname")
    output = merge(output, EnrichmentFootprint, by = "rowname")
    output = merge(output, StructureFootprint, by = "rowname")
    output = merge(output, HerbivoreFootprint, by = "rowname")
    output = merge(output, FungivoreFootprint, by = "rowname")
    output = merge(output, BacterivoreFootprint, by = "rowname")
    output = merge(output, PrOmFootprint, by = "rowname")
    output = merge(output, Numbers, by = "rowname")
    output = merge(output, CAssimilated, by = "rowname")
    output = merge(output, CRespired, by = "rowname")
    colnames(output)[1]="SampleID"
    output
  }
  output = index1(otutab, taxonomy)
  index@meta = merge(index@meta, output, by = "SampleID")
  results = merge(results, output, by = "SampleID")
  results = tibble::as_tibble(results)
  index@result = results
  return(index)
}
