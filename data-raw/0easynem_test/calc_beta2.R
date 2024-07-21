devtools::install_github("whkygl/easynem")
library(easynem)
?nem_plot
?`nem_plot,beta-method`
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_plot <- nem |>
  calc_beta(pcoa, Treatments, method = "bray") |>
  nem_plot(level = 0)
nem_plot
nem_plot <- nem |>
  calc_beta(nmds, Treatments, method = "bray") |>
  nem_plot(type = 2)
nem_plot
