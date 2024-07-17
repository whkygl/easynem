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
#' nemindex Class
#'
#' This class represents an extension of the \code{easynem} class.
#'
#' @slot result The calculation results of storage nemindex.
#' @export
methods::setClass(
  "nemindex",
  contains = "easynem",
  methods::representation(result = "data.frame")
)
methods::setMethod(
  "show",
  "nemindex",
  function(object) {
    cat("The nematode index of each treatment result is:\n")
    print(object@result)
  }
)
#' calc_nemindex
#' @description nematode index calculation for the tab.
#' @param data easynem type data.
#' @param ... Other parameters for calc_nemindex.
#' @return An nemindex object.
#' @export
calc_nemindex <- function(data, ...){
  # data = bac
  index = methods::new("nemindex")
  index@meta = data@meta
  index@tax = data@tax
  index@tab = data@tab
  result1 = index@tax
  results = data@meta[, sapply(data@meta, is.character)]
  otutab = index@tab
  taxonomy = index@tax[,1:8]
  nemdbgenus = index@tax[,c(1,9:34)]
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
    alltab = merge(otutab, taxonomy, by = names(otutab)[1], all.y = TRUE) |>
      merge(nemdbgenus, by = names(otutab)[1], all = FALSE)
    alltab$GenavgCRs = 0.273 * (alltab$GenavgMass ^ 0.75)
    alltab$GenavgCPr = 0.1 * alltab$GenavgMass / alltab$CP
    alltab$GenavgMFP = alltab$GenavgCRs + alltab$GenavgCPr
    alltab = alltab |> dplyr::mutate(GenavgEFP = ifelse(GenavgEFP != 0, GenavgMFP, GenavgEFP),
                                     GenavgSFP = ifelse(GenavgSFP != 0, GenavgMFP, GenavgSFP),
                                     GenavgHFP = ifelse(GenavgHFP != 0, GenavgMFP, GenavgHFP),
                                     GenavgFFP = ifelse(GenavgFFP != 0, GenavgMFP, GenavgFFP),
                                     GenavgBFP = ifelse(GenavgBFP != 0, GenavgMFP, GenavgBFP),
                                     GenavgPFP = ifelse(GenavgPFP != 0, GenavgMFP, GenavgPFP))
    tballtab = alltab[ ,c(names(otutab)[1],colnames(otutab)[-1])]
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
    alltab = alltab[, c(c(names(otutab)[1],colnames(otutab)[-1]), "Cp")]
    halltab = halltab[, c(c(names(otutab)[1],colnames(otutab)[-1]), "Cp")]
    sigalltab = sigalltab[, c(c(names(otutab)[1],colnames(otutab)[-1]), "Cp")]
    fbhalltab = fbhalltab[ , c(c(names(otutab)[1],colnames(otutab)[-1]), "Feeding")]
    bfpoalltab = bfpoalltab[ , c(c(names(otutab)[1],colnames(otutab)[-1]), "FunctionalGuild")]
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
