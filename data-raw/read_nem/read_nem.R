rm(list = ls())
# 定义S4的easynem类
setClass("easynem", 
    slots = list(
        tab = "data.frame",
        tax = "data.frame",
        meta = "data.frame"
    ))
.easynem <- new("easynem")
# 定义可向easynem类添加元素的read_tab函数
read_nem <- function(tab=0, tax=0, meta=0, ...){
  if(tab != 0){
    tab = readr::read_csv(tab, show_col_types = FALSE, ...)
    .easynem@tab = tab
  } else {
    warning("Otutab has not been imported yet")
  }
  if(tax != 0){
    tax = readr::read_csv(tax, show_col_types = FALSE, ...)
    .easynem@tax = tax
  } else {
    warning("Taxonomy has not been imported yet")
  }
  if(meta != 0){
    meta = readr::read_csv(meta, show_col_types = FALSE, ...)
    .easynem@meta = meta
  } else {
    warning("Metadata has not been imported yet")
  }
  return(.easynem)
}
# hehe <- read_nem(tab = "bacotu.csv", meta = "meta.csv", tax = "bactax.csv")
# hehe <- read_nem(tab = "bacotu.csv")
# hehe <- read_nem(tab = "bacotu.csv", meta = "meta.csv", col_names = FALSE)
# hehe <- read_nem()
# 重新改写S4对象"easynem"的show方法
setMethod("show", "easynem", function(object){
  cat("This is an easynem object\n")
  cat("The otutab is:\n")
  print(object@tab)
  cat("The taxonomy is:\n")
  print(object@tax)
  cat("The metadata is:\n")
  print(object@meta)
})
# show(hehe)