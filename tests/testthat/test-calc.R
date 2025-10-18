# -----------------------------------------------------------------------------
# Unit Tests: All calc_* Functions in the easynem Package
#
# Description:
# This script uses the 'testthat' package to verify the correctness of all
# core 'calc_*' functions in 'easynem'. The tests cover a range of
# calculations from alpha diversity to linear models.
# -----------------------------------------------------------------------------

library(testthat)
library(easynem)

# --- Data Preparation ---

# Load and prepare data for all tests to avoid redundant code.
# Single-factor dataset
nem_single <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)

# Two-factor dataset
nem_double <- read_nem(
  tab = easynem_example("nemtab1.csv"),
  tax = easynem_example("nemtax1.csv"),
  meta = easynem_example("nemmeta1.csv")
)

# Pre-calculate indices as they are required by multiple functions
nem_single_indexed <- calc_nemindex(nem_single)
nem_double_indexed <- calc_nemindex(nem_double)
nem_single_alpha_indexed <- calc_alpha(nem_single_indexed)

# --- Start of Tests ---

# Test: calc_alpha.R
test_that("calc_alpha correctly calculates alpha diversity indices", {
  alpha_div <- calc_alpha(nem_single)
  expect_s4_class(alpha_div, "alpha")
  expect_true(is.data.frame(alpha_div@result))
  expect_equal(nrow(alpha_div@result), nrow(nem_single@meta))
  expect_equal(ncol(alpha_div@result), 12) # SampleID + 6 metrics
})

# Test: calc_beta.R
test_that("calc_beta handles single-factor beta diversity with different ordination methods", {
  beta_pcoa <- calc_beta(nem_single, pcoa, Treatments, method = "bray")
  expect_s4_class(beta_pcoa, "beta")
  expect_equal(ncol(beta_pcoa@meta), 5)
  
  beta_nmds <- calc_beta(nem_single, nmds, Treatments, method = "bray")
  expect_s4_class(beta_nmds, "beta")
  expect_equal(ncol(beta_nmds@meta), 5)
})

# Test: calc_beta2.R
test_that("calc_beta2 correctly handles two-factor beta diversity", {
  beta2_pcoa <- calc_beta2(nem_double, pcoa, con_crop, season, method = "bray")
  expect_s4_class(beta2_pcoa, "beta2")
  expect_equal(ncol(beta2_pcoa@meta), 7) # SampleID, G1, G2, Interact, Axis1, Axis2
})

# Test: calc_compare.R
test_that("calc_compare performs single-factor comparison with different statistical methods", {
  compare_lsd <- calc_compare(nem_single, .group = Treatments, y = Mesorhabditis, method = LSD)
  expect_s4_class(compare_lsd, "compare")
  expect_named(compare_lsd@result[2], "LSD")
  
  compare_hsd <- calc_compare(nem_single, .group = Treatments, y = Mesorhabditis, method = HSD)
  expect_s4_class(compare_hsd, "compare")
  expect_named(compare_hsd@result[2], "HSD")
})

# Test: calc_compare2.R
test_that("calc_compare2 correctly handles two-factor comparisons", {
  compare2_lsd <- calc_compare2(nem_double, .group1 = con_crop, .group2 = season, y = pH, method = LSD2)
  expect_s4_class(compare2_lsd, "compare2")
  expect_true(is.data.frame(compare2_lsd@result))
  
  compare2_hsd <- calc_compare2(nem_double, .group1 = con_crop, .group2 = season, y = pH, method = HSD2)
  expect_s4_class(compare2_hsd, "compare2")
  expect_true(is.data.frame(compare2_hsd@result))
})

# Test: calc_ef.R
test_that("calc_ef correctly calculates single-factor energy flow", {
  ef <- calc_ef(nem_single_indexed, Treatments)
  expect_s4_class(ef, "ef")
  expect_true(is.data.frame(ef@result))
  expected_cols <- c("SampleID", "Treatments", "U", "OM", "BM", "HM", "FM", "fbo", "fho", "ffo", "frb", "frh", "frf")
  expect_true(all(expected_cols %in% names(ef@result)))
})

# Test: calc_ef2.R
test_that("calc_ef2 correctly handles two-factor energy flow", {
  ef2 <- calc_ef2(nem_double_indexed, con_crop, season)
  expect_s4_class(ef2, "ef2")
  expect_true(is.data.frame(ef2@result))
  expected_cols <- c("SampleID", "con_crop", "season", "U", "OM", "BM", "HM", "FM", "fbo", "fho", "ffo", "frb", "frh", "frf")
  expect_true(all(expected_cols %in% names(ef2@result)))
})

# Test: calc_funguild.R
test_that("calc_funguild correctly calculates single-factor functional guilds", {
  funguild <- calc_funguild(nem_single_indexed, Treatments)
  expect_s4_class(funguild, "funguild")
  expect_true(is.data.frame(funguild@result))
  expect_equal(ncol(funguild@result), 4)
  expect_true(all(c("SampleID", "Treatments", "EI", "SI") %in% names(funguild@result)))
})

# Test: calc_funguild2.R
test_that("calc_funguild2 correctly handles two-factor functional guilds", {
  funguild2 <- calc_funguild2(nem_double_indexed, con_crop, season)
  expect_s4_class(funguild2, "funguild2")
  expect_true(is.data.frame(funguild2@result))
  expect_equal(ncol(funguild2@result), 5)
})

# Test: calc_lm.R
test_that("calc_lm correctly performs single-factor linear regression", {
  lme <- calc_lm(nem_single_alpha_indexed, group = Treatments, x = Chao1, y = TotalBiomass)
  expect_s4_class(lme, "lme")
  expect_true(is.data.frame(lme@meta))
  expect_equal(ncol(lme@meta), 4)
  expect_true(all(c("SampleID", "Treatments", "Chao1", "TotalBiomass") %in% names(lme@meta)))
})

# Test: calc_lm2.R
test_that("calc_lm2 correctly performs two-factor linear regression", {
  lme2 <- calc_lm2(nem_double, con_crop, season, x = pH, y = Fe)
  expect_s4_class(lme2, "lme2")
  expect_true(is.data.frame(lme2@meta))
  expect_equal(ncol(lme2@meta), 5)
  expect_true(all(c("SampleID", "con_crop", "season", "pH", "Fe") %in% names(lme2@meta)))
})

# Test: calc_mf.R
test_that("calc_mf correctly calculates single-factor metabolic footprint", {
  mf <- calc_mf(nem_single_indexed, Treatments)
  expect_s4_class(mf, "mf")
  expect_true(is.data.frame(mf@result))
  expect_true(all(c("SampleID", "Treatments", "EI", "SI", "EnrichmentFootprint", "StructureFootprint") %in% names(mf@result)))
})

# Test: calc_mf2.R
test_that("calc_mf2 correctly handles two-factor metabolic footprint", {
  mf2 <- calc_mf2(nem_double_indexed, con_crop, season)
  expect_s4_class(mf2, "mf2")
  expect_true(is.data.frame(mf2@result))
  expect_true(all(c("SampleID", "con_crop", "season", "EI", "SI", "EnrichmentFootprint", "StructureFootprint") %in% names(mf2@result)))
})

# Test: calc_nemindex.R
test_that("calc_nemindex correctly calculates all nematode indices", {
  nem_index <- calc_nemindex(nem_single)
  expect_s4_class(nem_index, "nemindex")
  expect_true(is.data.frame(nem_index@result))
  expect_equal(nrow(nem_index@result), nrow(nem_single@meta))
})

# Test: calc_ter.R
test_that("calc_ter correctly generates single-factor ternary plot data", {
  ter <- calc_ter(nem_single, Treatments)
  expect_s4_class(ter, "ter")
  expect_true(is.data.frame(ter@result))
  expect_true(all(c("Herbivorous nematodes%", "Bacteria-feeding nematodes%", "Fungus-feeding nematodes%") %in% names(ter@result)))
  expect_true(all(c("cp1% (Enrichment)", "cp2% (Stress)", "cp3-5% (Stability)") %in% names(ter@result)))
})

# Test: calc_ter2.R
test_that("calc_ter2 correctly handles two-factor ternary plot analysis", {
  ter2 <- calc_ter2(nem_double, con_crop, season)
  expect_s4_class(ter2, "ter2")
  expect_true(is.data.frame(ter2@result))
  expect_true(all(c("con_crop", "season") %in% names(ter2@result)))
  expect_true("Herbivorous nematodes%" %in% names(ter2@result))
  expect_true("cp1% (Enrichment)" %in% names(ter2@result))
})
