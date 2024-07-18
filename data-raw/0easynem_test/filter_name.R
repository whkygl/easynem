devtools::install_github("whkygl/easynem")
library(easynem)
?filter_name
easynem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_filter <- easynem |> filter_name(target = meta, Treatments == "C4")
nem_filter
nem_filter <- easynem |> filter_name(target = tab, OTUID == "Mesorhabditis")
nem_filter
nem_filter <- easynem |> filter_name(target = tax, Family == "Rhabditidae")
nem_filter
