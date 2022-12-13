#' Builds an interactive data table from "data/test_output_table.rds"
#' for inclusion in the dashboard.
#' Includes some data manipulation and use of the DT for the table.
#' 
# Commented out to avoid double running in the pipeline.
# library(tidyverse)
# library(readxl)
# library(DT)
# library(lubridate)


# 1.1 Data manipulation -------------------------------------------------------

funct_fixed_colnums <- function(df){
  #' funct_fixed_colnums
  #' Find what the number of the columns are that are dates and fixed markers.
  #' This is so we can hide/perform operations easily on each group later.
  #'
  #' @param df of completed RFS schedule
  #' @return list: list element 1 is column numbers of 'fixed' columns, 
  #'  list element 2 is the same for date columns
  #'
  #' @examples fixed_colnums <- funct_fixed_colnums(rfs_table)[[1]]
  
  fixed_colnums <- seq(from = 1, 
                     to = length(names(df)))[str_detect(names(df), "fixed")]
  date_colnums <- fixed_colnums - 1
  colnums <- list(fixed_colnums, date_colnums)
  return (colnums)
}


funct_date_text <- function(df){
  #' funct_date_text
  #' Convert date outputs to form `yyyy-mm-dd` by converting them to text and
  #' taking a substring to remove the time element. 
  #' Also, fill empty 'fixed' columns with computed to help with colouring later.
  #' @param df of completed RFS schedule
  #' @return An identically shaped df where all dates are in form above.
  #' @examples rfs_table <- funct_date_text(rfs_table)
  
  df <- df %>%
    mutate_at(date_colnums, ~substr(., 1, 10)) %>%
    mutate_at(vars(contains("fixed")),
              ~replace(., is.na(.), "computed")) # Fill in blank cells with computed marker.
  return(df)
}

#  # 1.11 Handle forbidden dates ------------------------------------------

mark_forbidden_dates <- function(rfs_table, interval_series, date_colnums){
  
  #' mark_forbidden_dates
  #' Takes in series of time intervals and checks which dates in the dataframe 
  #' they apply to. If these dates are not 'fixed' they will be relabeld
  #' 'fobidden_dates'. This will allow the interactive table to be coloured by this. 
  #' @param rfs_table df of completed RFS schedule
  #' @param interval_series a series where each line is a lubridate interval object
  #' @return An identically shaped df where some milestones 'fixed' cols have been updated.
  #' @examples rfs_table <- mark_forbidden_dates(rfs_table, forbidden_ranges$inter)
  
  # Convert column of intervals into list so %within% can check all ranges at once
  forbidden_ranges = as.list(interval_series)
  
  # Reduce rfs table to only show phases that can be forbidden (PIN and award)
  rfs_table_copy <- rfs_table %>% mutate_at(date_colnums, ymd) %>% select(`PIN`, Award)
  
  # Create boolean table that states if dates are forbidden (T) or ok (F)
  forbidden_check <- rfs_table_copy %>% mutate_all(function(x) ymd(x) %within% forbidden_ranges)
  
  # List the rows that are forbidden for each
  rows_pin <- which(forbidden_check[,c("PIN")] == T)
  rows_award <- which(forbidden_check[,c("Award")] == T)
  
  # Compare these to their fixed tables, if they are fixed leave them, otherwise change marker to 'forbidden_date'
  if (length(rows_pin)>0){
    rfs_table[,c("PIN fixed")][rows_pin,] = case_when(rfs_table[,c("PIN fixed")][rows_pin,] == 'fixed' ~ "fixed",
                                                          TRUE ~ "forbidden_date")}
  if (length(rows_award)>0){
    rfs_table[,c("Award fixed")][rows_award,] = case_when(rfs_table[,c("Award fixed")][rows_award,] == 'fixed' ~ "fixed",
                                                          TRUE ~ "forbidden_date")}
  return(rfs_table)
}

# 1.2 Build table -------------------------------------------------------------

funct_build_table <- function(df, fixed_colnums, date_colnums){
  #' funct_build_table
  #' Build an interative data table for display. Specific features commented below.
  #' @param df of schedule, cleaned and ready to be displayed
  #' @param fixed_colnums, vector of column numbers of cols that are 'fixed' 
  #' @param date_colnums, as above for the correspoding date cols 
  #' @return DT (Data table) object
  #' @examples funct_build_table(rfs_table, fixed_colnums, date_colnums )
  
  rfs_int <- DT::datatable(rfs_table, 
                           fillContainer = TRUE,
                           escape = FALSE, # Allow HTML.
                           filter = "top", # Placement of filter boxes.
                           extensions = c("Buttons"), # Add standard buttons selection.
                           options = list(
                             autoWidth = TRUE, # Autosize column width.
                             columnDefs = list(list(visible=FALSE, targets=fixed_colnums)),
                             pageLength = 13, # Num entries on a page.
                             dom = "Bfrtip", # Layout components (Buttons, filter, r, table, i , pagination).
                             scrollX = TRUE, # Scroll bars
                             scrollY = TRUE,
                             scrollCollapse = TRUE,
                             initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '10px'});}"), # Override font size in header row using javascript.
                             buttons = c("copy", "excel")) # Choose which buttons to include.
  ) %>%
    
    DT::formatStyle(names(rfs_table), fontSize = "70%") %>% # Make font smaller. 
    DT::formatStyle(names(rfs_table),lineHeight="70%") 
  # Add colouring and bolding on each date col, based on the matching value in their date fixed column.
  for(i in 1:length(fixed_colnums)){
    rfs_int <- rfs_int %>%
      DT::formatStyle(columns = colnames(rfs_table[date_colnums][i]), 
                    valueColumns = colnames(rfs_table[fixed_colnums][i]),
                                     fontWeight = styleEqual(c("fixed","proposed", "computed","forbidden_date"),c("bold","normal", "normal","normal")),
                                     color = styleEqual(c("fixed","proposed", "computed", "forbidden_date"), c("black","black","teal","orange"))
    )
  }
  return(rfs_int)
}


# 2.0 Run functions -------------------------------------------------------
rfs_table <- rfs %>% select(Franchise, `Type`,  everything(), -ID) # Take cols of interest
fixed_colnums <- funct_fixed_colnums(rfs_table)[[1]] # Get col nums for smaller df
date_colnums <- funct_fixed_colnums(rfs_table)[[2]]
rfs_table <- funct_date_text(rfs_table) # Convert date display
rfs_table <- mark_forbidden_dates(rfs_table, forbidden_ranges$inter, date_colnums) # Add forbidden labels
rfs_int <- funct_build_table(rfs_table, fixed_colnums, date_colnums) # Build tables
