#' This file will test the functions used in data processing.
#' Each test function will be focus on a specific function in the data processing script.
require(testthat)
require(devtools)
source("00_data_processing.R")

# ---- Test readin input sheet -----#
col_names <- c("Franchise", "Company",                             
               "Iteration", "Pre Project Phase","Pre Project Phase fixed",             
               "Project Start","Project Start fixed","PIN/EoI",                             
               "PIN/EoI fixed","ITT/RfP","ITT/RfP fixed",                       
               "Award","Award fixed", "Franchise Start Date",                
               "Franchise Start Date fixed","Baseline Franchise Length (Yrs)",     
               "Baseline Franchise Length (RPs)", "Total Baseline Franchise Length (RPs)",
               "Baseline Rail Period Start Number", "Additional Rail Periods Called",      
               "Remaining Additional Rail Periods", "Franchise Rail Period Expiry Number",
               "Franchise Expiry Date", "Franchise Expiry Date fixed",         
               "Franchise Length RPS","Direct Award?","ID")

# each date column has a fixed date
test_that("Correct input column names", {
  expect_equal(paste0(colnames(input_df %>% select(date_colnums)), " fixed"), colnames(input_df %>% select(date_colnums + 1)))
})

# col names
test_that("Check date column is left of fixed column", {
  expect_named(input_df, col_names)
})

# each franchise has the same number of iterations
test_that("Check each franchise has the same number of iterations", {
  num_franch <- group_by(input_df, Franchise) %>% summarise(count = n()) %>% select(count) %>% unique()
  expect_equal(num_franch[[1]], max(input_df$Iteration))
})

# dimensions
test_that("Correct table dimensions",{
  expect_equivalent(nrow(input_df) %% length(unique(input_df$Franchise)), 0)
  expect_equivalent(ncol(input_df), length(col_names))
})

# Franchise startdate for each franchise set 1
test_that("First Franchise start dates are entered and formatted", {
  # no empty start dates for iteration 1
  expect_equivalent(sum(is.na(input_df[input_df[,"Iteration"] == 1,]$`Franchise Start Date`)), 0)
  # start dates for iteration 1 are class datetime
  expect_equivalent(sapply(input_df$`Franchise Start Date`, class)[1], "POSIXct")
  })


# ---- Test value update from missing assumption -----#
# Use global sliced_df[[3]] as output and df as input
#test_that("Check empty 'additional rail period called' are filled with 0", {
#  na_idx_num <- which(is.na(input_df[input_df[,"Iteration"] == 3,]$`Additional Rail Periods Called`))
#  if (!is_empty(na_idx_num)){
#    expect_equivalent(sliced_df[[3]][na_idx_num, "Additional Rail Periods Called"][[1,1]], 0)
#  }
#})

#test_that("Check empty 'Baseline Franchise Length (Yrs)' are filled with 8", {
#  na_idx_num <- which(is.na(input_df[input_df[,"Iteration"] == 3,]$`Baseline Franchise Length (Yrs)`))
#  if (!is_empty(na_idx_num)){
#    expect_equivalent(sliced_df[[3]][na_idx_num, "Baseline Franchise Length (Yrs)"][[1,1]], 8)
#  }
#})

#test_that("Check empty 'Remaining Additional Rail Periods' are filled with 26", {
#  na_idx_num <- which(is.na(input_df[input_df[,"Iteration"] == 3,]$`Remaining Additional Rail Periods`))
#  if (!is_empty(na_idx_num)){
#    expect_equivalent(sliced_df[[3]][na_idx_num, "Remaining Additional Rail Periods"][[1,1]], 26)
#  }
#})

#test_that("Check empty 'Total Baseline Franchise Length (RPs)' are filled with 104",{
#  na_idx_num <- which(is.na(input_df[input_df[,"Iteration"] == 3,]$`Total Baseline Franchise Length (RPs)`))
#  if (!is_empty(na_idx_num)){
#    expect_equivalent(sliced_df[[3]][na_idx_num, "Total Baseline Franchise Length (RPs)"][[1,1]], 104)
#  }
#})



# ---- Test complete table function -----#
# Use global sliced_df[[1]] as input for complete iteration
test_that("All dates filled", {
  expect_true(all(!is.na(rfs[,date_colnums])))
  expect_equal(lapply(rfs[,date_colnums], class) %>% unlist() %>% as.vector(), c("Date", "Date", "Date", "Date", "Date", "Date", "Date"))
})

test_that("table has correct dimensions", {
  expect_equivalent(dim(rfs)[1] %% length(unique(input_df$Franchise)), 0)
  expect_equivalent(dim(rfs)[2], length(col_names)+1)
})

# test that there are no available rps for contracts that have past the deadline
test_that("check there are no remaining aditional rail periods if the expiry date is closer than 3 months", {
  expect_equivalent(rfs$`Remaining Additional Rail Periods`[completed_df$`Months until RP extension deadline` < 0], 0)
})

