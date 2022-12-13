context("Test export RFS table to visio format")
source("1_export_visio_table.R")

require(testthat)

col_names <- c("Franchise", "Iteration", "Pre Project Phase",
               "Project Start", "PIN/EoI", "ITT/RfP",              
               "Award","Franchise Start Date","Franchise Expiry Date",
               "Direct Award?")


# Tests -------------------------------------------------------------------

# Input check
test_that("Input data contains correct columns",{
  expect_named(completed_df, col_names) 
  # expect_equal(lapply(df, typeof) %>% unlist() %>% as.vector(), 
  #              col_types)
})


# calculate month separation test


# Get franchise set


# convert months into pixels


# rename columns


# Test clearup ------------------------------------------------------------