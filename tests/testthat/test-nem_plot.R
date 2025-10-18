# install.packages("testthat") # Run if you don't have it installed

library(testthat)
library(easynem)

# --- Data Preparation ---

# Single-factor data
nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)

# Two-factor data from package example
nem2 <- read_nem(
  tab = easynem_example("nemtab1.csv"),
  tax = easynem_example("nemtax1.csv"),
  meta = easynem_example("nemmeta1.csv")
)

# --- Tests for nem_plot() ---

test_that("nem_plot works for beta-class", {
  beta_pcoa <- calc_beta(nem, pcoa, Treatments, method = "bray")
  p1 <- nem_plot(beta_pcoa, level = 0)
  p2 <- nem_plot(beta_pcoa, level = 0.6, type = 1)
  p3 <- nem_plot(beta_pcoa, level = 0.6, type = 2)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("nem_plot works for beta2-class", {
  beta2_pcoa <- calc_beta2(nem2, pcoa, con_crop, season, method = "bray")
  p1 <- nem_plot(beta2_pcoa, level = 0)
  p2 <- nem_plot(beta2_pcoa, level = 0.6, type = 1)
  p3 <- nem_plot(beta2_pcoa, level = 0.6, type = 2)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("nem_plot works for compare-class", {
  compare_lsd <- calc_compare(nem, .group = Treatments, y = Mesorhabditis, method = LSD)
  p1 <- nem_plot(compare_lsd, type = 1)
  p2 <- nem_plot(compare_lsd, type = 2, add = "mean_se")
  p3 <- nem_plot(compare_lsd, type = 2, add = "mean_sd")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("nem_plot works for compare2-class", {
  compare2_lsd <- calc_compare2(nem2, .group1 = con_crop, .group2 = season, y = pH, method = LSD2)
  p1 <- nem_plot(compare2_lsd, type1 = 1, type2 = 1)
  p2 <- nem_plot(compare2_lsd, type1 = 2, type2 = 2, add = "mean_sd")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("nem_plot works for funguild-class", {
  nem_with_index <- calc_nemindex(nem)
  funguild <- calc_funguild(nem_with_index, Treatments)
  p <- nem_plot(funguild)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for funguild2-class", {
  nem2_with_index <- calc_nemindex(nem2)
  funguild2 <- calc_funguild2(nem2_with_index, con_crop, season)
  p <- nem_plot(funguild2)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for mf-class", {
  nem_with_index <- calc_nemindex(nem)
  mf <- calc_mf(nem_with_index, Treatments)
  p <- nem_plot(mf, kei = 30, ksi = 20)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for mf2-class", {
  nem2_with_index <- calc_nemindex(nem2)
  mf2 <- calc_mf2(nem2_with_index, con_crop, season)
  p <- nem_plot(mf2, kei = 35, ksi = 35)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for ef-class", {
  nem_with_index <- calc_nemindex(nem)
  ef <- calc_ef(nem_with_index, Treatments)
  p <- nem_plot(ef)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for ef2-class", {
  nem2_with_index <- calc_nemindex(nem2)
  ef2 <- calc_ef2(nem2_with_index, con_crop, season)
  p <- nem_plot(ef2)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for ter-class", {
  ter <- calc_ter(nem, Treatments)
  p1 <- nem_plot(ter, type = feeding)
  p2 <- nem_plot(ter, type = cp)

  expect_s3_class(p1, "recordedplot")
  expect_s3_class(p2, "recordedplot")
})

test_that("nem_plot works for ter2-class", {
  ter2 <- calc_ter2(nem2, con_crop, season)
  p1 <- nem_plot(ter2, type = feeding)
  p2 <- nem_plot(ter2, type = cp)

  expect_s3_class(p1, "recordedplot")
  expect_s3_class(p2, "recordedplot")
})

test_that("nem_plot works for lme-class", {
  nem_alpha <- calc_alpha(nem)
  nem_index <- calc_nemindex(nem_alpha)
  lme <- calc_lm(nem_index, group = Treatments, x = Chao1, y = TotalBiomass)
  p <- nem_plot(lme)

  expect_s3_class(p, "ggplot")
})

test_that("nem_plot works for lme2-class", {
  lme2 <- calc_lm2(nem2, con_crop, season, x = pH, y = Fe)
  p <- nem_plot(lme2)

  expect_s3_class(p, "ggplot")
})
