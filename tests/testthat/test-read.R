# -----------------------------------------------------------------------------
# Unit Tests: Data Reading Functions in the easynem Package
#
# Description:
# This script uses the 'testthat' package to verify the correctness of the
# 'read_nem' and 'read_nem2' functions. The tests are based on the official
# documentation examples to ensure they behave as expected.
# -----------------------------------------------------------------------------

library(testthat)
library(easynem)

# --- Start of Tests ---

# Test: read_nem.R (based on documentation example)
test_that("read_nem correctly reads data from separate CSV files as per its example", {
  # The example uses easynem_example() to get the path to package data
  tab_file <- easynem_example("nemtab1.csv")
  tax_file <- easynem_example("nemtax1.csv")
  meta_file <- easynem_example("nemmeta1.csv")
  
  # Execute the function as shown in the example
  nem <- read_nem(tab = tab_file, tax = tax_file, meta = meta_file)
  
  # 1. Check if the returned object is of the S4 class 'nem'
  expect_s4_class(nem, "easynem")
  
  # 2. Verify that the slots are correctly populated and are data frames
  expect_true(is.data.frame(nem@tab))
  expect_true(is.data.frame(nem@tax))
  expect_true(is.data.frame(nem@meta))
  
  # 3. Check the dimensions of the imported data to ensure it was read correctly
  # (These numbers are based on the example data files)
  expect_equal(nrow(nem@tab), 145)
  expect_equal(ncol(nem@tab), 33) # 69 genera + 1 SampleID column
  expect_equal(nrow(nem@tax), 145)
  expect_equal(nrow(nem@meta), 32)
})

# Test: read_nem2.R (based on documentation example)
test_that("read_nem2 correctly reads data from internal data objects as per its example", {
  # The example loads data directly from the package's exported data frames
  # No file paths are needed. `testthat` will automatically load package data.
  nem <- read_nem2(tab = nemtab, tax = nemtax, meta = nemmeta)
  
  # 1. Check if the returned object is of the S4 class 'nem'
  expect_s4_class(nem, "easynem")
  
  # 2. Verify that the slots are correctly populated and are data frames
  expect_true(is.data.frame(nem@tab))
  expect_true(is.data.frame(nem@tax))
  expect_true(is.data.frame(nem@meta))
  
  # 3. Check the dimensions of the imported data
  # (These numbers are based on the built-in nemtab, nemtax, and nemmeta data)
  expect_equal(nrow(nem@tab), 46)
  expect_equal(ncol(nem@tab), 13) # 21 genera + 1 SampleID column
  expect_equal(nrow(nem@tax), 46)
  expect_equal(nrow(nem@meta), 12)
})
