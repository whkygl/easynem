devtools::install_github("whkygl/easynem")
library(easynem)
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_index <- nem |>
  calc_alpha() |>
  calc_nemindex() |>
  calc_lm(group = Treatments,
          x = Chao1,
          y = TotalBiomass)

nem <- read_nem(tab = easynem_example("nemtab1.csv"),
                tax = easynem_example("nemtax1.csv"),
                meta = easynem_example("nemmeta1.csv"))
nem_lm <- nem |> calc_lm2(con_crop, season, x = pH, y = Fe)
