devtools::install_github("whkygl/easynem")
library(easynem)
?nem_plot
?`beta2-class`
?`nem_plot,beta2-method`
? calc_beta2
nem <- read_nem(tab = easynem_example("nemtab1.csv"),
                tax = easynem_example("nemtax1.csv"),
                meta = easynem_example("nemmeta1.csv"))
nem_plot <- nem |>
  calc_beta2(pcoa, con_crop, season, method = "bray") |>
  nem_plot()
nem_plot
nem_plot <- nem |>
  calc_beta2(nmds, con_crop, season, method = "bray") |>
  nem_plot(type = 2)
nem_plot
