devtools::install_github("whkygl/easynem")
library(easynem)
?trans_combine
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_trans <- nem |>
  trans_name(cp_value) |>
  trans_norm(method = percent) |>
  trans_combine(c("3", "4", "5"))
show(nem_trans)
nem_trans@meta$`3_4_5`
