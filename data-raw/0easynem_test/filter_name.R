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
easynem <- read_nem(tab = easynem_example("nemtab1.csv"),
                    tax = easynem_example("nemtax1.csv"),
                    meta = easynem_example("nemmeta1.csv"))
nem_filter <- easynem |> filter_name(target = meta, season == "Summer")
nem_filter
nem_filter <- easynem |> filter_name(target = tab, OTUID == "ASV_5")
nem_filter
nem_filter <- easynem |> filter_name(target = tax, Genus == "Meloidogyne")
nem_filter
