library(testthat)
library(readxl)
context("Test functionality to let users change phase length assumptions")

# Tests -------------------------------------------------------------------
test_that("Expected output is generated when code is run",{
  # Pull in test input, test assumptions and expected output.
  # Run them through the schedule and change code as planned and check output
  # is as expected. 
  # Input - Some dates fixed, others - milestones sep by 1m, contract length 10 years
  # Changed assumption - milestones sep by 2m, contract length 1 year.
  
  ## !!!! If data_processing uses months instead of 28 day periods then the input test data sheet, 
  ## expected output will need to updated to continue to match. !!!!
  
  filepath <- "data/inputs/schedule_input_test.xlsx"
  out_filename <- "schedule_baseline_test"
  rp_lookup_table <- read_excel(filepath, sheet = "Lookup")
  assumption_table <- read_excel(filepath, sheet = "Assumptions")
  source("00_data_processing.R")
  rfs_before <- rfs
  rm(completed_df)
  source("01_assumptions_change.R")
  rm(input_df, alt_assumptions_table)
  
  rfs_changed_expected <- read_excel(filepath, sheet="Expected_output",
                                     col_types = c("text", "text", "numeric", 
                                                              "date", "text", "date", "text", "date", 
                                                              "text", "date", "text", "date", "text", 
                                                              "date", "text", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "date", "text", 
                                                              "numeric", "text", "text")) %>% 
    mutate_at(date_colnums, as.Date)

  expect_true(all(rfs_changed_expected$`Franchise Start Date` == rfs_changed$`Franchise Start Date`))
  expect_true(all(rfs_changed_expected$Award == rfs_changed$Award))
  expect_true(all(rfs_changed_expected$`ITT/RfP` == rfs_changed$`ITT/RfP`))
  expect_true(all(rfs_changed_expected$`PIN/EoI` == rfs_changed$`PIN/EoI`))
  expect_true(all(rfs_changed_expected$`Project Start` == rfs_changed$`Project Start`))
  
  expect_true(all(rfs_changed_expected$`Pre Project Phase` == rfs_changed$`Pre Project Phase`))
})
