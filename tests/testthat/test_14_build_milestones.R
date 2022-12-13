context("Test fact tab inputs, processing and outputs")
source("2_prep_markdown.R")
source("1_build_milestones.R")
library(testthat)

# Tests -------------------------------------------------------------------
test_that("Input data contains correct columns",{
  
  col_names <- c("Franchise", "Iteration", "Pre Project Phase",
                 "Project Start", "PIN/EoI", "ITT/RfP",              
                 "Award","Franchise Start Date","Franchise Expiry Date",
                 "Direct Award?", "id")
  
  col_types <- c("character", "double", "double",
                 "double", "double", "double",
                 "double", "double", "double",
                 "character", "charachter")
  
  expect_named(df, col_names) 
  expect_equal(lapply(df, typeof) %>% unlist() %>% as.vector(), 
               col_types)
  test_path(paste0(filepath_output, ".rds"))
  test_path(paste0(filepath_output,"_changed" ,".rds"))
})



test_that("Planned plots are plotly objects", {
  
  expect_equal(class(milestone_fig), c("plotly", "htmlwidget"))
  expect_equal(class(milestone_fig_changed), c("plotly", "htmlwidget"))
  expect_equal(length(milestone_fig$x$attrs$`39cc94d694d`$hovertemplate), 
    nrow(milestone_table))
  expect_equal(length(milestone_fig_changed$x$attrs$`39cc94d694d`$hovertemplate), 
               nrow(milestone_table_changed))
  
})


test_that("Baseline and changed input dataframes are different", {
  
  expect_false(all.equal(rfs, rfs_changed))
  expect_false(all.equal(milestone_table, milestone_table_changed))
  expect_false(all.equal(milestone_fig$x$attrs$`39cc94d694d`$hovertemplate, 
                         milestone_fig_changed$x$attrs$`39cc94d694d`$hovertemplate))
  
})


test_that("Congestion function gives right period of congestion", {
  ITT_dates <- data.frame(`ITT-RfP date` = c("2010-01-01","2011-01-01","2011-01-02",
                                             "2012-01-01","2012-02-01")) %>%
    mutate(`ITT-RfP date` = as.Date(ITT.RfP.date))

  congestion <- func_calc_ITT_congestion_range(ITT_dates, 2) %>% as.character()
  
  expect_equal(congestion, c("2011-01-01","2011-01-02") %>% as.character())
  
})
