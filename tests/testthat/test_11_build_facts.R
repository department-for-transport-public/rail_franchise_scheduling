context("Test fact tab inputs, processing and outputs")
source("11_build_facts.R")
# Test preperation --------------------------------------------------------

library(testthat)

col_names <- c("Franchise", "Iteration", "Pre Project Phase",
               "Project Start", "PIN/EoI", "ITT/RfP",              
               "Award","Franchise Start Date","Franchise Expiry Date",
               "Direct Award?")

col_types <- c("character", "double", "double",
               "double", "double", "double",
               "double", "double", "double",
               "character")

input_agg_miss <- data.frame(
  "DA"= c("Competition", "Competition"),
  "PP" = c(10, 20),
  "PS" = c(100,200)
)
colnames(input_agg_miss) <- c("Direct Award?","Phase: Pre Project Phase (RPs)","Phase: Franchise Start Date (RPs)")

# Tests -------------------------------------------------------------------
test_that("Input data contains correct columns",{
    expect_named(facts_df, col_names) 
    expect_equal(lapply(facts_df, typeof) %>% unlist() %>% as.vector(), 
                 col_types)
    })


test_that("Aggregation works when there is only one category available",{
          expect_equal(func_average_phases(input_agg_miss)[[1]], "Competition")
          expect_equal(func_average_phases(input_agg_miss)[[2]], 15)
          expect_equal(func_average_phases(input_agg_miss)[[3]], 150)
          })


test_that("No NA's appear before aggregation",
  expect_true(all(!is.na(df_phases)))
  )


test_that("No NA's appear during aggregation",
 expect_true(all(!is.na(df_aggregated)))
 )


test_that("Test for output data table dimensions",
  expect_equal(ncol(phase_table$x$data), 8)
)

