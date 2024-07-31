devtools::install_github("whkygl/easynem")
library(easynem)
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_plot <- nem |> calc_compare(.group = Treatments,
                                y = Mesorhabditis,
                                method = LSD) |>
  nem_plot()
nem_plot
nem@meta$Treatments <- factor(nem@meta$Treatments, levels = c("CK","C2","C4","C8"))
nem_plot <- nem |> calc_compare(.group = Treatments,
                                y = Mesorhabditis,
                                method = LSD) |>
  nem_plot()
nem_plot
nem <- read_nem(tab = easynem_example("nemtab1.csv"),
                tax = easynem_example("nemtax1.csv"),
                meta = easynem_example("nemmeta1.csv"))
nem_plot <- nem |>
  calc_nemindex() |>
  calc_ef2(con_crop, season) |>
  nem_plot()
nem_plot
nem@meta$con_crop <- factor(nem@meta$con_crop, levels = c("Y2","Y5","Y8","Y11"))
nem@meta$season <- factor(nem@meta$season, levels = c("Summer","Spring"))
nem_plot <- nem |>
  calc_nemindex() |>
  calc_ef2(con_crop, season) |>
  nem_plot()
nem_plot
nem_plot <- nem |>
  calc_nemindex() |>
  calc_mf(Treatments) |>
  nem_plot(kei = 30, ksi = 20)
nem_plot
