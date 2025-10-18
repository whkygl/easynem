# -----------------------------------------------------------------------------
# Unit Tests: All filter_* Functions in the easynem Package
#
# Description:
# This script uses the 'testthat' package to verify the correctness of all
# data filtering ('filter_*') functions in 'easynem'. The tests are based
# on the official documentation examples.
# -----------------------------------------------------------------------------

library(testthat)
library(easynem)

# --- Data Preparation ---

# Load data once to be used in all tests
nem_base <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)

# --- Start of Tests ---

# Test: filter_name.R (based on documentation example)
test_that("filter_name correctly filters by categorical variable as per its example", {
  # Example: nem_filter <- nem |> filter_name(Treatments, c("OT", "CK"))
  nem_filtered <- filter_name(nem_base, meta, Treatments == "C4")
  
  # Check the class of the output
  expect_s4_class(nem_filtered, "easynem")
  
  # Verify that the 'Treatments' column in the filtered metadata only contains "OT" and "CK"
  unique_treatments <- unique(nem_filtered@meta$Treatments)
  expect_true(unique_treatments == "C4")
})

# Test: filter_num.R (based on documentation example)
test_that("filter_num correctly filters by a numerical range as per its example", {

  nem_filtered <- filter_num(nem_base, num = 1000)
  
  # Check the class of the output
  expect_s4_class(nem_filtered, "easynem")
  
  expect_true(unique(rowSums(nem_filtered@tab[,-1]) >= 1000))
})
