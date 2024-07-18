devtools::install_github("whkygl/easynem")
library(easynem)
?filter_num
easynem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_filter <- easynem |> filter_num(num = 0.95)
show(nem_filter)
nem_filter <- easynem |> filter_num(num = 1400)
show(nem_filter)
