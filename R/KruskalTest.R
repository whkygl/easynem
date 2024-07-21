#' Perform Kruskal-Wallis test on easynem meta-table by treatment (single factor)
#'
#' The \code{KruskalTest()} is used to perform \code{Kruskal-Wallis} test for
#' grouped data and create \code{\link{compare-class}}. This function is only
#' applicable to single factor analysis, see \code{\link{KruskalTest2}} for a
#' two factor version of the function.
#'
#' To facilitate code interpretation, It is recommended to use this function in
#' conjunction with the \code{\link{calc_compare}} function:
#'
#' ```
#' nem_compare <- nem |> calc_compare(.group = con_crop, y = pH, method = KruskalTest)
#' ```
#'
#' @usage KruskalTest(data, .group, y, exact=FALSE, sort=TRUE, .method=c("holm",
#' "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), ...)
#'
#' @param data An \code{\link{easynem-class}} data.
#' @param .group Grouping variables.
#' @param y Dependent variable (numeric data).
#' @param exact logical. If TRUE, calculate exact Wilcoxon tests. Default \code{exact = FALSE}.
#' @param sort logical. If TRUE, sort groups by median dependent variable values.
#' Default \code{sort = TRUE}.
#' @param .method method for correcting p-values for multiple comparisons.
#' @param ... Other parameters for \code{\link[stats]{kruskal.test}}.
#'
#' @return An \code{\link{compare-class}} object.
#'
#' @seealso
#' Other functions related to differential analysis methods: \code{\link{TTest2}},
#' \code{\link{TTest}}, \code{\link{WilcoxTest2}}, \code{\link{WilcoxTest}},
#' \code{\link{KruskalTest2}}, \code{\link{LSD}}, \code{\link{LSD2}}, \code{\link{HSD}},
#' \code{\link{HSD2}}.
#'
#' @references
#' R in Action: Data Analysis and Graphics with R, Second Edition by Robert I. Kabacoff,
#' published by Manning Publications. 178 South Hill Drive, Westampton, NJ 08060 USA.
#' Copyright 2015 by Manning Publications.
#'
#' @export
#' @examples
#' nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
#' nem_test <- nem |>
#'             calc_compare(.group = Treatments,
#'               y = Mesorhabditis,
#'               method = KruskalTest)
#' nem_test
KruskalTest <- function(data, .group, y, exact=FALSE, sort=TRUE,
                        .method=c("holm", "hochberg", "hommel", "bonferroni",
                                  "BH", "BY", "fdr", "none"), ...){
  .compare = methods::new("compare")
  y2 = y
  meta = as.data.frame(data@meta)
  meta = meta[,c(names(meta)[1], .group, y)]
  row.names(meta) = meta[,1]
  method <- match.arg(.method)
  df = meta
  y <- df[[3]]
  g <- as.factor(df[[2]])
  vnames <- names(df)
  if(sort) g <- stats::reorder(g, y, FUN=stats::median)
  groups <- levels(g)
  k <- nlevels(g)
  getstats <- function(x)(c(N = length(x), Median = stats::median(x),
                            MAD = stats::mad(x)))
  sumstats <- t(stats::aggregate(y, by=list(g), FUN=getstats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  kw <- stats::kruskal.test(meta[[y2]]~meta[[.group]], data=meta, ...)
  wmc <- NULL
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      y1 <- y[g==groups[i]]
      y2 <- y[g==groups[j]]
      test <- stats::wilcox.test(y1, y2, exact=exact)
      r <- data.frame(Group.1=groups[i], Group.2=groups[j],
                      W=test$statistic[[1]], p=test$p.value)
      wmc <- rbind(wmc, r)
    }
  }
  wmc$p <- stats::p.adjust(wmc$p, method=method)


  data <- data.frame(y, g)
  names(data) <- c(vnames[3],vnames[2])
  results <- list(CALL = match.call(),
                  data=data,
                  sumstats=sumstats, kw=kw,
                  method=method, wmc=wmc, vnames=vnames)
  .compare@result = results
  .compare@meta = meta
  .compare@temp = c("KruskalTest")
  return(.compare)
}
