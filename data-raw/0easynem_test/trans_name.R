devtools::install_github("whkygl/easynem")
library(easynem)
?trans_name
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_trans <- nem |> trans_name(Family)
show(nem_trans)
nem_trans <- nem |> trans_name(feeding)
show(nem_trans)
nem_trans@meta[,c("1", "2", "3", "5", "8")]
