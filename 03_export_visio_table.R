#' Output table to Visio.
#' - Output a set of Franchises from the RFS table into Visio table format.
#' - For Visio to produce a milestone Gantt chart it needs the milestone's date (e.g. Jun-2020) and the distance (mm) 
#' the milestone is placed on the chart.
#' - DAs should not be included in the visio table as they are manually define in the Visio app.
#' 
#' Code structure
#' --------------
#' - The `funct_find_first_start_date()` pulls out the first Franchise contract from a user-defined origin date. The
#' default origin date is set to today's date.
#' - The nearest Franchise contract is pulled out from the chart's start date. This defines the Franchise set which
#'  will be displayed on the Visio Gantt.
#' - The monthly distance between each milestone is calculated for each franchise contract.
#' - This distance is converted to a pixel distance, which is capped at 410 mm.
#' - The start date of each milestone is formatted as June-2020.
#' - Then the table is write to a CSV file for the user to feed into Visio.
#' TODO:
#' * cbind the Franchise onto the final table output.

# Commented out to avoid double running in the pipeline.
# library(lubridate)
# source('0_data_processing.R')


funct_find_first_start_date <- function(df, chart_start = today()) {
  # Find the start date nearest to today.
  #' Find out the first start date which can be used as a reference.
  #' @param df - RFS table
  #' @return an origin start date
  
  df %>% filter(`Franchise Start Date` > chart_start & `Type` == "Competition") %>%
    select(`Franchise Start Date`) %>% arrange(`Franchise Start Date`) %>% slice(1)
  
}


funct_get_franchise_set <- function(df, chart_start = today()) {
  # Function that collect first 14 franchises.
  # Sort 14 franchise by today's date.
  #' @param df - RFS table
  #' @return a table of 14 unique franchises sorted from today's date by Franchise Start date
  
 df %>% filter(`Franchise Start Date` > chart_start & Type == "Competition") %>% 
    arrange(Franchise, `Franchise Start Date`) %>%
    group_by(Franchise) %>% slice(1) %>% ungroup()
}


monnb <- function(d) {
  # Turn a date into a 'monthnumber' relative to an origin.
  lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  lt$year * 12 + lt$mon
}


# Compute a month difference as a difference between two monnb's.
mondf <- function(d1, d2) {
  monnb(d2) - monnb(d1)
}


funct_calc_month_separation <- function(df, origin_date) {
  # Function calcs month separation from first start date.
  #' Converts dates differences into months for the entire rfs table and returns the 1st 14 franchise set.
  #' @param df: complete rfs table with all rows
  #' @param origin_date: first contract start date which is closest to today's date
  #' @return month calendar data frame

  funct_get_franchise_set(df) %>%
    select(func_get_date_colnums(df)) %>%
    mutate_all(function(x){mondf(origin_date[[1]], x)}) %>%
    mutate_all(function(x){replace(x, which(x < 0), 0)})
  
}


# Function convert months to pixel (1 mon == 4.5 pix).
funct_convert_to_pixel <- function(df) {
  #' Converts the month difference into pixel ratio and thresholds at 410.
  #'  @param df: month difference table set
  df <- df * 4.5
  df %>%
    mutate_all(function(x){replace(x, which(x > 410), 410)}) 
}


funct_rename_cols <- function(pixel_df, rfs_dates) {
  #' Combines all the relevant date tables and formats thes column names.
  #' @param pixel_df: pixel columns for the milestone dates
  #' @param rfs_dates: formatted RFS dates
  #' @return df in visio format with rows [5mm, Jul 2018, 0mm, Jul 2019,...]

  data.frame(
    PQQ = paste0(pixel_df$`PQQ`, " mm"),
    PQQ = rfs_dates$`PQQ`,
    e = paste0(pixel_df$`PIN`, " mm"),
    e = rfs_dates$`PIN`,
    I = paste0(pixel_df$`ITT/RfP`, " mm"),
    I = rfs_dates$`ITT/RfP`,
    A = paste0(pixel_df$`Award`, " mm"),
    A = rfs_dates$Award,
    S = paste0(pixel_df$`Franchise Start`, " mm"),
    S = rfs_dates$`Franchise Start Date`,
    E = paste0(pixel_df$`Franchise Expiry Date`, " mm"),
    E = rfs_dates$`Franchise Expiry Date`,
    stringsAsFactors = FALSE
  )
  
}


funct_format_date_output <- function(df) {
  #' Formats date col from 15/10/2018 to Oct 2018.
  #' @param df: dataframe with date columns for milestone date columns.
  #' @return the same dataframe with dates formatted correctly.
  df %>% mutate_all(function(x){format(x, "%b %Y")})

}


visio_main <- function() {
  #' Reshape df `rfs` from 0_data_processing to form visio can ingest.
  #' @return CSV "data/sched_for_visio.csv" with one row for each franchise and a 
  #' pixel length and date for each milestone.i.e. [5mm, Jul 2018, 0mm, Jul 2019,...]
  
  
  fc_set_df <- funct_calc_month_separation(df = rfs, origin_date = funct_find_first_start_date(rfs))
  # exclude date colnums for pre-proj and proj-start
  relevant_dates_df <- funct_get_franchise_set(rfs) %>% 
    select(date_colnums[-1:-2]) %>%
    funct_format_date_output(.)
  pixel_df <- funct_convert_to_pixel(fc_set_df)
  visio <- funct_rename_cols(pixel_df, rfs_dates = relevant_dates_df)
  write_csv(visio, "data/outputs/schedule_visio.csv")
  return(visio)
}

 visio_main()
 
# rm(main, funct_format_date_output, funct_rename_cols,funct_convert_to_pixel, funct_calc_month_separation, monnb, mondf, funct_get_franchise_set,funct_find_first_start_date)
