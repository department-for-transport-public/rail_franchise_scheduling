# Tests for DA change code.

source("00_data_processing.R")
source("01_changes_functions.R")
source("02_changes_run.R")
library(testthat)

# Tests -------------------------------------------------------------------

test_that("rfs df is of the same dimensions as rfs_changed df after changes have been run.", {
  expect_true(all.equal(dim(rfs),dim(rfs_changed)))
  expect_true(all.equal(sapply(rfs, class),sapply(rfs_changed, class)))
})


test_that("find_matching_row returns a valid integer row number when given a test ID.", {
  expect_true(find_matching_row("Franchise 01-1",rfs)>0 && find_matching_row("Franchise 01-1",rfs)<=length(rfs$Franchise))
  expect_true(is.integer(find_matching_row("Franchise 01-1",rfs)))
})



test_that("locate_fixed_cols returns a list of length 7", {
  expect_true(length(locate_fixed_cols(rfs))==7)
})


test_that("new da row outputs a row of dimensions 1,28", {
  test_change <- changes[0,] %>%
    add_row('Franchise' = "Franchise 01", "Iteration" = 6, "Change type" = "Add DA"
            , 'Item to change' = "DA-Emergency", "Change" = 12, "Date Change" = NA
            , "ID" = "Franchise 01-6")
  
  expect_true(all.equal(dim(new_da_row(test_change,rfs)),c(1,28)))
  rm(test_change)
})

test_that("contract extensions extends a row expiry date by 2 RP of dimensions 1,28", {
  test_change <- changes[0,] %>%
    add_row('Franchise' = "Franchise 01", "Iteration" = 6, "Change type" = "Contract extension"
            , 'Item to change' = "Extension", "Change" = 2, "Date Change" = NA
            , "ID" = "Franchise 01-6")
  old_contract <- rfs[rfs[,'ID'] == 'Franchise 01-6',]
  expect_true(all.equal(dim(extend_RP(rfs, test_change)),c(1,28)))
  # updated rp called
  expect_true(all.equal(extend_RP(rfs, test_change)$`Additional Rail Periods Called`, old_contract$`Additional Rail Periods Called` + 2))
  expect_true(all.equal("changed", extend_RP(rfs, test_change)$`Franchise Expiry Date fixed`))
  
  rm(test_change, old_contract)
})

test_that("change baseline contract length a rows contract length changes to 10 yrs of dimensions 1,28", {
  test_change <- changes[0,] %>%
    add_row('Franchise' = "Franchise 01", "Iteration" = 6, "Change type" = "Contract length change"
            , 'Item to change' = "Extension", "Change" = 10, "Date Change" = NA
            , "ID" = "Franchise 01-6")
  old_contract <- rfs[rfs[,'ID'] == 'Franchise 01-6',]
  
  expect_true(all.equal(dim(change_contract_length(rfs, test_change)),c(1,28)))
  expect_true(all.equal(change_contract_length(rfs, test_change)$`Baseline Franchise Length (Yrs)`, 10))
  expect_true(all.equal("changed", change_contract_length(rfs, test_change)$`Franchise Expiry Date fixed`))
  
  rm(test_change, old_contract)
})
