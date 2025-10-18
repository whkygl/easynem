# -----------------------------------------------------------------------------
# Unit Tests: All trans_* Functions in the easynem Package
#
# Description:
# This script uses the 'testthat' package to verify the correctness of all
# data transformation ('trans_*') functions in 'easynem'.
# -----------------------------------------------------------------------------

library(testthat)
library(easynem)

# --- Data Preparation ---

# Load data once to be used in all tests
nem_base <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)

# --- Start of Tests ---

# Test: trans_combine.R
test_that("trans_combine correctly merges two nem objects", {

  # Combine them
  nem_combined <- trans_combine(nem_base, c("Mesorhabditis", "Rhabditis"))
  
  # Check the class of the output
  expect_s4_class(nem_combined, "easynem")
  
  expect_true("Mesorhabditis_Rhabditis" %in% names(nem_combined@meta))
  expect_true(all(nem_combined@meta$Mesorhabditis_Rhabditis == nem_base@meta$Mesorhabditis + nem_base@meta$Rhabditis))  
  # Ensure the number of columns remains the same
  expect_equal(ncol(nem_combined@tab), ncol(nem_base@tab))
})

# Test: trans_formula_v.R
test_that("trans_formula_v correctly applies a formula to the meta slot", {
  # Add a new variable based on existing ones
  nem_transformed <- trans_formula_v(nem_base, nem_base@tab$OTUID, ~ log(x + 1))
  
  # Check the class of the output
  expect_s4_class(nem_transformed, "easynem")
  
  # Verify the calculation is correct for the first row
  expect_equal(nem_transformed@meta$Mesorhabditis[1], log(nem_base@meta$Mesorhabditis[1] + 1))
  expect_equal(nem_transformed@meta$Rhabditis[2], log(nem_base@meta$Rhabditis[2] + 1))
})

# Test: trans_formula.R
test_that("trans_formula correctly applies a formula to the tab slot", {

  nem_transformed <- trans_formula(nem_base, Mesorhabditis, ~log(x + 1))
  
  # Check the class of the output
  expect_s4_class(nem_transformed, "easynem")
  
  # Verify the calculation is correct for a specific row
  expect_equal(nem_transformed@meta$Mesorhabditis[1], log(nem_base@meta$Mesorhabditis[1] + 1))
  expect_equal(nem_transformed@meta$Mesorhabditis[5], log(nem_base@meta$Mesorhabditis[5] + 1))
})

# Test: trans_name.R
test_that("trans_name correctly renames a column", {
  # Rename the 'pH' column to 'Soil_pH'
  nem_renamed <- trans_name(nem_base, Family)
  
   # Check the class of the output
  expect_s4_class(nem_renamed, "easynem")
  
  expect_true(all(nem_base@tax$Family %in% colnames(nem_renamed@meta)))
})

# Test: trans_norm.R
test_that("trans_norm correctly normalizes abundance data", {
  nem_normalized <- trans_norm(nem_base, method = percent)
  
   # Check the class of the output
  expect_s4_class(nem_normalized, "easynem")
  
  expect_equal(sum(colSums(nem_normalized@tab[,-1])), 1200)
})

# Test: trans_rare.R
test_that("trans_rare correctly rarefies the data", {
  nem_rare = trans_rare(nem_base)
  
  # Check the class of the output
  expect_s4_class(nem_rare, "easynem")
  
  expect_equal(sum(colSums(nem_rare@tab[,-1])), 17844)
})
