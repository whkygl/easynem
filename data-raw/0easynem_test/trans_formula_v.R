devtools::install_github("whkygl/easynem")
library(easynem)
?trans_formula_v
easynem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_trans <- easynem |> trans_formula_v(easynem@tab$OTUID, ~log(x+1))
nem_trans
