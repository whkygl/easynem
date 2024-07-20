devtools::install_github("whkygl/easynem")
library(easynem)
?trans_rare
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_trans <- nem |> trans_rare()
nem_trans
colSums(nem_trans@tab[,-1])
nem_trans <- nem |> trans_rare(1500)
nem_trans
colSums(nem_trans@tab[,-1])
