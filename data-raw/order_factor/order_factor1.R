devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), tax = easynem_example("bactax.csv"), meta = easynem_example("meta.csv"))
order_factor <- function(data, group, order){
    meta = data@meta
    group = deparse(substitute(group))
    # 选取meta中的group列转换为因子型并按向量order的顺序重新排序
    meta[[group]] = factor(meta[[group]], levels = order)
    data@meta = meta
    return(data)
}
hehe <- bac |> calc_compare(con_crop, pH, method = LSD) |> nem_plot()
hehe <- bac |> order_factor(con_crop, c("Y2", "Y5", "Y8", "Y11")) |> calc_compare(con_crop, pH, method = LSD) |> nem_plot()
