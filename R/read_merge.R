# nemdbfamily = readr::read_csv("nemdbfamily.csv")
# nemdbgenus = readr::read_csv("nemdbgenus.csv")
# nemdbspecies = readr::read_csv("nemdbspecies.csv")
# 定义tab类对象并进行验证
validate_tab <- function(object){
  errors <- c(
    if (class(object[[1]]) != "character")
      "the taxonomy name in the first column must be of character type"
    else if (!all(sapply(object, typeof)[-1] == "double"))
      "all numbers in the species abundance table must be double, check whether NA exists in the dataframe")
  if (length(errors) == 0) TRUE else errors
}
#' An S4 class to represent otutab.
setClass("tab", contains = "data.frame", validity = validate_tab)
# 定义tax类对象并进行验证
validate_tax <- function(object){
  errors <- c(
    if (ncol(object) != 8)
      "make sure that the classification system is a level 7 classification system"
    else if (!all(sapply(object, typeof) == "character"))
      "the species classification name must be of character type")
  if (length(errors) == 0) TRUE else errors
}
#' An S4 class to represent species classification table.
setClass("tax", contains = "data.frame", validity = validate_tax)
# 定义design类对象并进行验证
validate_design <- function(object){
  errors <- c(
    if (!all(sapply(object, typeof) == "character"))
      "The test design name must be of character type")
  if (length(errors) == 0) TRUE else errors
}
#' An S4 class to represent experimental design table.
setClass("design", contains = "data.frame", validity = validate_design)
# 定义output类对象并进行验证
validate_output <- function(object){
  errors <- c(
    if (class(object[[1]]) != "character")
      "the sample name in the first column must be of character type"
    # else if (!all (sapply(object, typeof)[-1] == "double"))
    #   "all numbers in the output table must be double, check whether NA exists in the dataframe"
    )
  if (length(errors) == 0) TRUE else errors
}
#' An S4 class to represent other result data table.
setClass("output", contains = "data.frame", validity = validate_output)
# 定义env类对象并进行验证
validate_env <- function(object){
  errors <- c(
    if (class(object[[1]]) != "character")
      "the sample name in the first column must be of character type"
    else if (!all(sapply(object, typeof)[-1] == "double"))
      "all numbers in the envrionmental factors table must be double, check whether NA exists in the dataframe")
  if (length(errors) == 0) TRUE else errors
}
#' An S4 class to represent table of environmental factors.
setClass("env", contains = "data.frame", validity = validate_env)
# 验证easynem类
# validate_easynem <- function(object){
#   errors <- c(
#     if (class(object@tab)[1] != "tab")
#       "easynem's species abundance table must be tab class"
#     else if (class(object@tax)[1] != "tax")
#       "easynem's species classification table must be tax class"
#     else if (class(object@design)[1] != "design")
#       "easynem's test design table must be design class"
#     else if (class(object@env)[1] != "env")
#       "easynem's environmental factors table must be env class")
#   if (length(errors) == 0) TRUE else errors
# }
# S4类，定义easynem类，nem类继承easynem类
#' An S4 class to represent microbial community structure data set.
#'
#' @slot tab Otutab.
#' @slot tax Species classification table.
#' @slot design Experimental design table.
#' @slot output Other result data table.
#' @slot temp Temporary data file.
#' @slot env Environmental factors.
setClass("easynem",
         slots = list(
           tab = "tab",
           tax = "tax",
           design = "design",
           output = "output",
           temp = "list",
           env = "env"
         ))
#' An S4 class to represent nematode community structure data set.
#'
#' @slot tab Otutab.
#' @slot tax Species classification table.
#' @slot design Experimental design table.
#' @slot output Other result data table.
#' @slot temp Temporary data file.
#' @slot env Environmental factors.
#' @slot nemdb Nematode function index reference database.
setClass("nem", slots = list(nemdb = "list"), contains = "easynem")
# tab对象实例化函数并同时实例化easynem对象并返回
# bac <- new("easynem") # 新建easynem实例
# nem <- new("nem") # 新建nem实例

#' @title read_tab: Generic functions related to reading into otutab
#'
#' @param object Microbial data type: easynem or nem.
#' @param tab The string associated with the file name.
#' @param ... Readr package read_csv function other parameters
#'
#' @return Returns a microbiome dataset of type easynem containing properties such as tab, tax, design, env, etc.
#' @export
#'
#' @usage read_tab(object, tab, ...)
setGeneric("read_tab", function(object, tab, ...) standardGeneric("read_tab"))

#' A read_tab method written for easynem
#'
#' @param object easynem.
#' @param tab character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return easynem.
#' @export
setMethod("read_tab", signature(object = "easynem", tab = "character"), function(object, tab, ...) {
  # Name = as.character(substitute(object))
  ftab = new("tab", readr::read_csv(tab, show_col_types = FALSE))
  object@tab <- ftab
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' A read_tab method written for nem
#'
#' @param object nem.
#' @param tab character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return nem.
#' @export
setMethod("read_tab", signature(object = "nem", tab = "character"), function(object, tab, ...) {
  # Name = as.character(substitute(object))
  ftab = new("tab", readr::read_csv(tab, show_col_types = FALSE))
  object@tab <- ftab
  object@nemdb <- list(family = nemdbfamily, genus = nemdbgenus, species = nemdbspecies)
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' @title read_tax: Generic functions related to reading into species classification table
#'
#' @param object Microbial data type: easynem or nem.
#' @param tax The string associated with the file name.
#' @param ... Readr package read_csv function other parameters
#'
#' @return Returns a microbiome dataset of type easynem containing properties such as tab, tax, design, env, etc.
#' @export
#'
#' @usage read_tax(object, tax, ...)
setGeneric("read_tax", function(object, tax, ...) standardGeneric("read_tax"))

#' A read_tax method written for easynem
#'
#' @param object easynem.
#' @param tax character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return easynem.
#' @export
setMethod("read_tax", signature(object = "easynem", tax = "character"), function(object, tax, ...) {
  # Name = as.character(substitute(object))
  ftax = new("tax", readr::read_csv(tax, show_col_types = FALSE))
  object@tax <- ftax
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' A read_tax method written for nem
#'
#' @param object nem.
#' @param tax character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return nem.
#' @export
setMethod("read_tax", signature(object = "nem", tax = "character"), function(object, tax, ...) {
  # Name = as.character(substitute(object))
  ftax = new("tax", readr::read_csv(tax, show_col_types = FALSE))
  object@tax <- ftax
  object@nemdb <- list(family = nemdbfamily, genus = nemdbgenus, species = nemdbspecies)
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' @title read_design: Generic functions related to reading into experimental design table
#'
#' @param object Microbial data type: easynem or nem.
#' @param design The string associated with the file name.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return Returns a microbiome dataset of type easynem containing properties such as tab, tax, design, env, etc.
#' @export
#'
#' @usage read_design(object, design, ...)
setGeneric("read_design", function(object, design, ...) standardGeneric("read_design"))

#' A read_design method written for easynem
#'
#' @param object easynem.
#' @param design character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return easynem.
#' @export
setMethod("read_design", signature(object = "easynem", design = "character"), function(object, design, ...) {
  # Name = as.character(substitute(object))
  fdesign = new("design", readr::read_csv(design, show_col_types = FALSE))
  object@design <- fdesign
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' A read_design method written for nem
#'
#' @param object nem.
#' @param design character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return nem.
#' @export
setMethod("read_design", signature(object = "nem", design = "character"), function(object, design, ...) {
  # Name = as.character(substitute(object))
  fdesign = new("design", readr::read_csv(design, show_col_types = FALSE))
  object@design <- fdesign
  object@nemdb <- list(family = nemdbfamily, genus = nemdbgenus, species = nemdbspecies)
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})
# setGeneric("read_output", function(object, output, ...) standardGeneric("read_output"))
# setMethod("read_output", signature(object = "easynem", output = "character"), function(object, output, ...) {
#   # Name = as.character(substitute(object))
#   foutput = magrittr::`%>%`(readr::read_csv(output, show_col_types = FALSE), new("output", .))
#   object@output <- foutput
#   object@envname <- colnames(object@output)[-1]
#   # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
#   return(object)
# })
# setMethod("read_output", signature(object = "nem", output = "character"), function(object, output, ...) {
#   # Name = as.character(substitute(object))
#   foutput = magrittr::`%>%`(readr::read_csv(output, show_col_types = FALSE), new("output", .))
#   object@output <- foutput
#   object@envname <- colnames(object@output)[-1]
#   object@nemdb <- nemdb
#   # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
#   return(object)
# })

#' @title read_env: Generic functions related to reading into table of environmental factors
#'
#' @param object Microbial data type: easynem or nem.
#' @param env The string associated with the file name.
#' @param ... Readr package read_csv function other parameters
#'
#' @return Returns a microbiome dataset of type easynem containing properties such as tab, tax, design, env, etc.
#' @export
#'
#' @usage read_env(object, env, ...)
setGeneric("read_env", function(object, env, ...) standardGeneric("read_env"))

#' A read_env method written for easynem
#'
#' @param object easynem.
#' @param env character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return easynem.
#' @export
setMethod("read_env", signature(object = "easynem", env = "character"), function(object, env, ...) {
  # Name = as.character(substitute(object))
  # env = "env.csv"
  fenv = new("env", readr::read_csv(env, show_col_types = FALSE))
  foutput = new("output", readr::read_csv(env, show_col_types = FALSE))
  # bac@env <- fenv
  # bac@output <- foutput
  object@env <- fenv
  object@output <- foutput
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})

#' A read_env method written for nem
#'
#' @param object nem.
#' @param env character.
#' @param ... Readr package read_csv function other parameters.
#'
#' @return nem.
#' @export
setMethod("read_env", signature(object = "nem", env = "character"), function(object, env, ...) {
  # Name = as.character(substitute(object))
  fenv = new("env", readr::read_csv(env, show_col_types = FALSE))
  foutput = new("output", readr::read_csv(env, show_col_types = FALSE))
  object@env <- fenv
  object@output <- foutput
  object@nemdb <- list(family = nemdbfamily, genus = nemdbgenus, species = nemdbspecies)
  # suppressWarnings(assign(Name, object, envir = .GlobalEnv))
  return(object)
})
# read_tab(bac, tab = "bacotu.csv")
# read_tax(bac, tax = "bactax.csv")
# read_design(bac, design = "design.csv")
# env_read(bac, env = "env.csv")
# 重写S4 easynem/nem的展示函数show()

#' Easynem's show general function
#'
#' @param object Microbial community data set.
#'
#' @return Print out the easynem object in the console window.
#' @export
setMethod("show", "easynem", function(object) {
  cat('An object of class "easynem"\n')
  cat('@tab\n')
  cat('Slot "tab": Species abundance table\n')
  show(object@tab)
  cat('@tax\n')
  cat('Slot "tax": Species classification table\n')
  show(object@tax)
  cat('@design\n')
  cat('Slot "design": Experimental design table\n')
  show(object@design)
  cat('@env\n')
  cat('Slot "env": Environmental factor\n')
  show(object@env)
  cat('@output\n')
  cat('Slot "output": Output table\n')
  show(object@output)
  cat('@temp\n')
  cat('Slot "temp": Temp table\n')
  show(object@temp)
})
#' Nem's show method
#'
#' @param object Nematode community structure data set.
#'
#' @return Print out the nem object details in the console window.
#' @export
setMethod("show", "nem", function(object) {
  callNextMethod(object)
  cat('@nemdb\n')
  cat('Slot "nemdb": Nematode database file, the specific content can be typed into nemdb() for detailed view\n')
})

#' Easynem's summary general function
#'
#' @param object Microbial data type: easynem or nem.
#' @param ... Others.
#'
#' @return Print out the easynem object details in the console window.
#' @export
#'
#' @usage summary(object, ...)
setGeneric("summary", function(object, ...) standardGeneric("summary"))


#' Easynem's summary method
#'
#' @param object Microbial data type: easynem.
#'
#' @return Print out the easynem object details in the console window.
#' @export
setMethod("summary", "easynem", function(object) {
  cat("Counts:\n")
  cat(ncol(object@tab) - 1, "samples\t", nrow(object@tab), "species\t", ncol(object@env) - 1, "output\t", ncol(object@design) - 1, "grouping variables\n")
  cat("Total number of species for each sample:\n")
  print(colSums(object@tab[,-1]))
  cat("Distribution:\n")
  summary(colSums(object@tab[,-1]))
})

# save(merge, nem, bac, fun, file = "test.RData")
# bac <- new("easynem") |> read_tab("bacotu.csv") |> read_tax("bactax.csv") |> read_design("design.csv") |> read_env("env.csv")
# fun <- new("easynem") |> read_tab("funotu.csv") |> read_tax("funtax.csv") |> read_design("design.csv") |> read_env("env.csv")
# nem <- new("nem") |> read_tab("nemotu.csv") |> read_tax("nemtax.csv") |> read_design("design.csv") |> read_env("env.csv")
# hehe <- read_merge(nem,bac,fun)
