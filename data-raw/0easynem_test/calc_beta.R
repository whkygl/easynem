devtools::install_github("whkygl/easynem")
library(easynem)
?calc_beta
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
nem_pcoa <- nem |> calc_beta(pcoa, Treatments, method = "bray")
show(nem_pcoa)
nem_nmds <- nem |> calc_beta(nmds, Treatments, method = "bray")
show(nem_nmds)
nem_pca <- nem |> calc_beta(pca, Treatments, method = "bray")
show(nem_pca)
nem2 <- read_nem(tab = easynem_example("nemtab1.csv"),
                    tax = easynem_example("nemtax1.csv"),
                    meta = easynem_example("nemmeta1.csv"))
nem2_pcoa <- nem2 |> calc_beta(pcoa, con_crop, method = "bray")
show(nem2_pcoa)
nem2_nmds <- nem2 |> calc_beta(nmds, con_crop, method = "bray")
show(nem2_nmds)
nem2_pca <- nem2 |> calc_beta(pca, con_crop, method = "bray")
show(nem2_pca)
