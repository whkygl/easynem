################################################################################
#' The main experiment-level class for easynem data
#'
#' Integrate the nematode abundance table, nematode classification table, and
#' experimental design table into an \code{easynem-class}, which makes it easier to
#' filter and manage nematode data, and easier to link to the nematode database
#' and conduct subsequent analysis.
#'
#' Users can read data via \code{\link{read_nem}} or \code{\link{read_nem2}}.
#' When there are missing slots in easynem, the system will issue a warning, but
#' this will not affect subsequent analysis.
#'
#'@slot tab A single object of nematode abundance table.
#'@slot tax A single object of nematode classification table.
#'@slot meta A single object of experimental design table.
#'
#'@seealso
#' The constructor, \code{\link{read_nem}} for reading csv files and
#' \code{\link{read_nem2}} for reading tibble type data.
#'
#' @name easynem-class
#' @rdname easynem-class
#' @exportClass easynem
methods::setClass("easynem",
                  slots = list(
                    tab = "data.frame",
                    tax = "data.frame",
                    meta = "data.frame"
                  ))
methods::setMethod("show", "easynem", function(object){
  cat("This is an easynem object\n")
  cat("The otutab is:\n")
  print(object@tab)
  cat("The taxonomy is:\n")
  print(object@tax)
  cat("The metadata is:\n")
  print(object@meta)
})
################################################################################
#' A class to store beta diversity results (single factor)
#'
#' \code{beta-class} is used to store the results of beta diversity analysis,
#' including results for drawing and comparing differences between groups.
#'
#' Users can construct a \code{beta-class} through \code{\link{calc_beta}},
#' which can then be connected to \code{\link{nem_plot}} to visualize the results.
#'
#' @slot meta A data frame storing basic elements for visualization.
#' @slot result A character of pairwise comparison results.
#' @slot temp A character vector of the difference comparison.
#'
#' @seealso
#' The constructor, \code{\link{calc_beta}}; Class for storing two-factor beta
#' diversity analysis, \code{\link{beta2-class}}; Visualization function,
#' \code{\link{nem_plot}}.
#'
#' @name beta-class
#' @rdname beta-class
#' @exportClass beta
methods::setClass("beta",
                  slots = list(
                    meta = "data.frame",
                    result = "character",
                    temp = "character"
                  ))
methods::setMethod("show", "beta", function(object){
  cat("This is an beta object\n")
  print(object@meta)
  cat("The result of the difference comparison is\n")
  print(object@result)
})
################################################################################
#' A class to store beta diversity results (two-factor)
#'
#' \code{beta2-class} is used to store the results of beta diversity analysis,
#' including results for drawing and comparing differences between groups.
#'
#' Users can construct a \code{beta2-class} through \code{\link{calc_beta2}},
#' which can then be connected to \code{\link{nem_plot}} to visualize the results.
#'
#' @slot meta A data frame storing basic elements for visualization.
#' @slot result A character of pairwise comparison results.
#' @slot temp A character vector of the difference comparison.
#'
#' @seealso
#' The constructor, \code{\link{calc_beta2}}; Class for storing single factor beta
#' diversity analysis, \code{\link{beta-class}}; Visualization function,
#' \code{\link{nem_plot}}.
#'
#' @name beta2-class
#' @rdname beta2-class
#' @exportClass beta2
methods::setClass("beta2",
                  slots = list(
                    meta = "data.frame",
                    result = "character",
                    temp = "character"
                  ))
methods::setMethod("show", "beta2", function(object){
  cat("This is an beta2 object\n")
  print(object@meta)
  cat("The result of the difference comparison is\n")
  print(object@result)
})
################################################################################
#' A S4 class to store multiple comparisons results (single factor).
#'
#' \code{compare-class} is used to store the results of multiple comparisons results,
#' including results for drawing and comparing differences between groups.
#'
#' Users can construct a \code{compare-class} through \code{\link{calc_compare}},
#' which can then be connected to \code{\link{nem_plot}} to visualize the results.
#'
#' @slot meta A data frame storing basic elements for visualization.
#' @slot result A data frame of multiple comparisons results.
#' @slot temp A character vector of the difference comparison.
#'
#' @seealso
#' The constructor, \code{\link{calc_compare}}; Class for storing two-factor
#' multiple comparisons analysis, \code{\link{compare2-class}}; Visualization function,
#' \code{\link{nem_plot}}.
#'
#' @name compare-class
#' @rdname compare-class
#' @exportClass compare
methods::setClass("compare",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY",
                    temp = "character"
                  ))
methods::setMethod("show", "compare", function(object){
  cat("This is an compare object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store multiple comparisons results (two-factor).
#'
#' \code{compare2-class} is used to store the results of multiple comparisons results,
#' including results for drawing and comparing differences between groups.
#'
#' Users can construct a \code{compare2-class} through \code{\link{calc_compare2}},
#' which can then be connected to \code{\link{nem_plot}} to visualize the results.
#'
#' @slot meta A data frame storing basic elements for visualization.
#' @slot result A data frame of multiple comparisons results.
#' @slot temp A character vector of the difference comparison.
#'
#' @seealso
#' The constructor, \code{\link{calc_compare2}}; Class for storing single factor
#' multiple comparisons analysis, \code{\link{compare-class}}; Visualization function,
#' \code{\link{nem_plot}}.
#'
#' @name compare2-class
#' @rdname compare2-class
#' @exportClass compare2
methods::setClass("compare2",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY",
                    temp = "character"
                  ))
methods::setMethod("show", "compare2", function(object){
  cat("This is an compare2 object\n")
  cat("The difference comparison is:\n")
  print(object@result)
})
################################################################################
#' Class for storing alpha diversity calculation results
#'
#' The \code{alpha-class} is an extension of the \code{\link{easynem-class}} to store
#' the results of alpha diversity calculations.
#'
#' @slot result The calculation results of storage alpha diversity.
#'
#' @seealso
#' The constructor, \code{\link{calc_alpha}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name alpha-class
#' @rdname alpha-class
#' @exportClass alpha
methods::setClass(
  "alpha",
  contains = "easynem",
  methods::representation(result = "data.frame")
)
methods::setMethod(
  "show",
  "alpha",
  function(object) {
    cat("The alpha diversity of each treatment result is:\n")
    print(object@result)
  }
)
################################################################################
#' Class for storing nematode ecological index calculation results
#'
#' The \code{nemindex-class} is an extension of the \code{\link{easynem-class}} to store
#' the results of nematode ecological index calculations.
#'
#' @slot result The calculation results of storage nematode ecological index.
#'
#' @seealso
#' The constructor, \code{\link{calc_nemindex}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name nemindex-class
#' @rdname nemindex-class
#' @exportClass nemindex
methods::setClass(
  "nemindex",
  contains = "easynem",
  methods::representation(result = "data.frame")
)
methods::setMethod(
  "show",
  "nemindex",
  function(object) {
    cat("The nematode index of each treatment result is:\n")
    print(object@result)
  }
)
################################################################################
#' Class for storing computational results of nematode functional guild analysis (single factor)
#'
#' The \code{funguild-class} is used to store the results of nematode functional guild analysis.
#'
#' @slot result A data frame of storing computational results of nematode
#' functional guild analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_funguild}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name funguild-class
#' @rdname funguild-class
#' @exportClass funguild
methods::setClass("funguild",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "funguild", function(object){
  cat("This is a funguild object\n")
  cat("The functional guilds of the nematode communities in each treatment were:\n")
  print(object@result)
})
################################################################################
#' Class for storing computational results of nematode functional guild analysis (two-factor)
#'
#' The \code{funguild2-class} is used to store the results of nematode functional guild analysis.
#'
#' @slot result A data frame of storing computational results of nematode
#' functional guild analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_funguild2}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name funguild2-class
#' @rdname funguild2-class
#' @exportClass funguild2
methods::setClass("funguild2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "funguild2", function(object){
  cat("This is a funguild2 object\n")
  cat("The functional guilds of the nematode communities in each treatment were:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store Metabolic footprints results (single factor)
#'
#' The \code{mf-class} is used to store the results of nematode metabolic footprints analysis.
#'
#' @slot result A data frame for storing the results of metabolic footprinting analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_mf}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name mf-class
#' @rdname mf-class
#' @exportClass mf
methods::setClass("mf",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "mf", function(object){
  cat("This is an mf object\n")
  cat("The metabolic footprints of the nematode communities in each treatment were:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store Metabolic footprints results (two-factor)
#'
#' The \code{mf2-class} is used to store the results of nematode metabolic footprints analysis.
#'
#' @slot result  A data frame for storing the results of metabolic footprinting analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_mf2}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name mf2-class
#' @rdname mf2-class
#' @exportClass mf2
methods::setClass("mf2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "mf2", function(object){
  cat("This is an mf2 object\n")
  cat("The metabolic footprints of the nematode communities in each treatment were:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store energy flow results (single factor)
#'
#' The \code{ef-class} is an extension of the \code{\link{easynem-class}} to store
#' the results of nematode energy flow analysis.
#'
#' @slot result A data frame for storing the results of energy flow analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_ef}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name ef-class
#' @rdname ef-class
#' @exportClass ef
methods::setClass(
  "ef",
  contains = "easynem",
  methods::representation(result = "data.frame")
  )
methods::setMethod("show", "ef", function(object){
  cat("This is an ef object\n")
  cat("The energy flow results is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store energy flow results (two-factor)
#'
#' The \code{ef2-class} is an extension of the \code{\link{easynem-class}} to store
#' the results of nematode energy flow analysis.
#'
#' @slot result A data frame for storing the results of energy flow analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_ef2}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name ef2-class
#' @rdname ef2-class
#' @exportClass ef2
methods::setClass(
  "ef2",
  contains = "easynem",
  methods::representation(result = "data.frame")
)
methods::setMethod("show", "ef2", function(object){
  cat("This is an ef object\n")
  cat("The energy flow results is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store the ternary analysis results (single factor)
#'
#' The \code{ter-class} is used to store the results of nematode ternary analysis.
#'
#' @slot result A data frame for storing the results of ternary analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_ter}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name ter-class
#' @rdname ter-class
#' @exportClass ter
methods::setClass("ter",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "ter", function(object){
  cat("This is an ter object\n")
  cat("The ternary analysis results is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store the ternary analysis results (two-factor)
#'
#' The \code{ter2-class} is used to store the results of nematode ternary analysis.
#'
#' @slot result A data frame for storing the results of ternary analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_ter2}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name ter2-class
#' @rdname ter2-class
#' @exportClass ter2
methods::setClass("ter2",
                  slots = list(
                    result = "data.frame"
                  ))
methods::setMethod("show", "ter2", function(object){
  cat("This is an ter2 object\n")
  cat("The ternary analysis results is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store the linear regression analysis results (single factor)
#'
#' The \code{lme-class} is used to store the results of linear regression analysis.
#'
#' @slot meta Stores the data frame for plotting.
#' @slot result A data frame for storing the results of linear regression analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_lm}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name lme-class
#' @rdname lme-class
#' @exportClass lme
methods::setClass("lme",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY"
                  ))
methods::setMethod("show", "lme", function(object){
  cat("This is an lme object\n")
  cat("The Linear regression analysis results is:\n")
  print(object@result)
})
################################################################################
#' A S4 class to store the linear regression analysis results (two-factor)
#'
#' The \code{lme2-class} is used to store the results of linear regression analysis.
#'
#' @slot meta Stores the data frame for plotting
#' @slot result A data frame for storing the results of linear regression analysis.
#'
#' @seealso
#' The constructor, \code{\link{calc_lm2}}; Visualization function, \code{\link{nem_plot}}.
#'
#' @name lme2-class
#' @rdname lme2-class
#' @exportClass lme2
methods::setClass("lme2",
                  slots = list(
                    meta = "data.frame",
                    result = "ANY"
                  ))
methods::setMethod("show", "lme2", function(object){
  cat("This is an lme2 object\n")
  cat("The Linear regression analysis results is:\n")
  print(object@result)
})
################################################################################
