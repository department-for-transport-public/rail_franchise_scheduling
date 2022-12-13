require(tidyverse)
require(dplyr)
require(readxl)
require(lubridate)

# 1.0 Functions ---------------------------------------------------------------
# 1.1 pull parameter functions-------------------------------------------------

# 1.1 update column functions-------------------------------------------------
# Calculate the rail period expiry number using the baseline rail period start number and total contract length
func_update_expiry_RP_num <- function(input_table) {
  #' Converts years into rail periods then updates the expiry rail period number using
  #' the rp start number and the contract length in rps.
  #' @param input_table: input datasheet with cols "Baseline Franchise Length (Yrs)", 
  #' "Baseline Franchise Length (RPs)", "Additional Rail Periods Called"
  #' @return datatable with update expiry rp number.
  input_table <- mutate(input_table,
                        `Total Baseline Franchise Length (RPs)` = 13 * `Baseline Franchise Length (Yrs)` + `Baseline Franchise Length (RPs)`,
                        `Franchise Length RPS` = `Total Baseline Franchise Length (RPs)` + `Additional Rail Periods Called`,
                        `Franchise Rail Period Expiry Number` = `Baseline Rail Period Start Number` + `Franchise Length RPS`
  )
  return(input_table)
}


# Use assumption sheet to fill in missing values on RFS table
func_update_empty_values <- function(df, assumption_tab) {
  #' Use the assumption parameters to fill missing cells in the rfs table.
  #' @param df: input rfs datasheet.
  #' @param assumpt_tabl: assumption table from the input datasheet
  #' @return rfs table with complete columns.
  #' 
  
  # Different lengths of default for each contract type
  baseline_length_yrs <- case_when(
    df$Type == "DA-Emergency" ~ filter(assumption_tab, ref == "R3")$value,
    df$Type == "DA-Planned" ~ filter(assumption_tab, ref == "R2")$value,
    TRUE ~ filter(assumption_tab, ref == "R1")$value)
  
  max_rps <- filter(assumption_tab, ref == "O22")$value # This is the default flexibility a contract has to extend.
  df$`Baseline Franchise Length (Yrs)` <- case_when(is.na(df$`Baseline Franchise Length (Yrs)`) ~ baseline_length_yrs,
                                                    TRUE ~  df$`Baseline Franchise Length (Yrs)`)
  # Take the default contract length from above, but only if the current value is missing.
  df$`Baseline Franchise Length (RPs)` <- replace_na(df$`Baseline Franchise Length (RPs)`, 0)
  
  # Update all other columns needed to generate the schedule.
  df$`Total Baseline Franchise Length (RPs)` <- case_when(
    is.na(df$`Total Baseline Franchise Length (RPs)`) ~ df$`Baseline Franchise Length (Yrs)` *13,
    TRUE ~  df$`Total Baseline Franchise Length (RPs)`)
  
  df$`Additional Rail Periods Called` <- replace_na(df$`Additional Rail Periods Called`, 0)
  df$`Remaining Additional Rail Periods` <- replace_na(df$`Remaining Additional Rail Periods`, max_rps)
  return(df)
}

func_update_empty_dates <- function(df, assumption_tab) {
  #' Uses the phase duration parameters to fill/update missing date values in the rfs table.
  #' @param df: the incomplete rfs with missing date entries.
  #' @param durations: a vector of the pulled assumptions for phase durations, in the order of:
  #' pre proj, proj start, pin, itt, start date.
  #' @return completed date columns in the RFS table.
  for (x in 1:length(df$Award)) {
    # Depending on contract type find the assumptions sheet reference prefix.
    if(df$`Type`[x] =="DA-Planned"){
      prefix <- "DAP"
      } else if (df$`Type`[x] =="DA-Emergency"){
      prefix <- "EDAP"
      }else {
      prefix <- "S"
    }
  
    # For each phase, if missing, apply the suggested phase gaps in the assumptions sheet.
    # This will give us a whole set of dates.
      df$Award[x] <- replace_na(df$Award[x], df$`Franchise Start Date`[x] %m+% months(
        -assumption_tab[assumption_tab$ref == paste0(prefix,6), ][["value"]]))
      
      df$`ITT/RfP`[x] <- replace_na(df$`ITT/RfP`[x], df$Award[x] %m+% months(
        -assumption_tab[assumption_tab$ref== paste0(prefix,5), ][["value"]]))
      
      df$`PQQ`[x] <- replace_na(df$`PQQ`[x], df$`ITT/RfP`[x] %m+% months(
        -assumption_tab[assumption_tab$ref == paste0(prefix,4), ][["value"]]))
      
      df$`PIN`[x] <- replace_na(df$`PIN`[x], df$`PQQ`[x] %m+% months(
        -assumption_tab[assumption_tab$ref == paste0(prefix,3), ][["value"]]))
      
      df$`Project Start`[x] <- replace_na(df$`Project Start`[x], df$`PIN`[x] %m+% months(
        -assumption_tab[assumption_tab$ref == paste0(prefix,2), ][["value"]]))
      
      df$`Pre Project Phase`[x] <- replace_na(df$`Pre Project Phase`[x], df$`Project Start`[x] %m+% months(
        -assumption_tab[assumption_tab$ref == paste0(prefix,1), ][["value"]]))
  }
  return(df)
}


# Return datetime column ----------------------------------------------------
func_get_date_colnums <- function(df) {
  fixed_colnums <- seq(
    from = 1,
    to = length(names(df))
  )[str_detect(names(df), "fixed")]
  date_colnums <- fixed_colnums - 1
  # colnums <- list(fixed_colnums, date_colnums)
  return(date_colnums)
}


complete_iteration <- function(input_table, assumption_tab, rp_look_up_tab) {
  # Calculate the rail period expiry number using the baseline rail period start number and total contract length
  input_table <- func_update_expiry_RP_num(input_table)
  
  # Use look-up table to convert RP start number to contract start date
  join <- select(input_table, `Baseline Rail Period Start Number`) %>%
    left_join(rp_look_up_tab, by = c("Baseline Rail Period Start Number" = "Rail Period Number"))
  input_table$`Franchise Start Date` <- join$RP
  
  # Use look-up table to convert RP expiry number to contract end date
  join <- select(input_table, `Franchise Rail Period Expiry Number`) %>%
    left_join(rp_look_up_tab, by = c("Franchise Rail Period Expiry Number" = "Rail Period Number"))
  input_table$`Franchise Expiry Date` <- join$RP
  
  # format date columns and removed invalid datetime entries
  date_colnums <- func_get_date_colnums(input_table)
  input_table  <- input_table %>% mutate_at(date_colnums[1:(length(date_colnums))], function(x) 
    as.Date(as.POSIXct(x, format = "%Y-%m-%d", origin = "1970-01-01", tz = "UTC")))
  
  # Fill in missing milestone dates using assumption sheet
  input_table <- func_update_empty_dates(df=input_table, assumption_tab)
  
  return(input_table)
}

complete_table <- function(df, assumption_tab) {
  # Sort by franchise and iteration.
  df <- df %>%
    arrange(Franchise, Iteration) %>%
    mutate(Type = replace_na(Type, "Competition"))
  
  # Count the number of unique franchises for the schedule
  franchises <- length(unique(df$Franchise))
  
  # Use assumption sheet to fill in missing values on RFS table
  df <- func_update_empty_values(df, assumption_tab)
  
  # Split df into schedule set (using split instead of group split to fit with citrix dplyr)
  sliced_df <- split(df, df$Iteration)
  
  # Iterate through each schedule set and complete dates/update RP number
  for (x in 1:length(unique(df$Iteration))) {
    # complete dates for each schedule set
    sliced_df[[x]] <- complete_iteration(sliced_df[[x]], assumption_tab, rp_lookup_table)
    
    # condition to stop at the last iteration
    if (x == length(unique(df$Iteration))) {
      break
    }
    
    # update next schedule set's RP number. Join performed so that groups of inequal size can be handled.
    sliced_df[[x+1]] <- sliced_df[[x+1]] %>% left_join(
        sliced_df[[x]] %>% select(Franchise, `Franchise Rail Period Expiry Number`), 
        by = "Franchise") %>%
      # If the match isn't there, then keep what you have. Otherwise collect new date.
      mutate(`Baseline Rail Period Start Number` = ifelse(is.na(`Franchise Rail Period Expiry Number.y`),
                                                          `Baseline Rail Period Start Number`,
                                                          `Franchise Rail Period Expiry Number.y`)) %>%
      select(-`Franchise Rail Period Expiry Number.y`,
              -`Franchise Rail Period Expiry Number.x`)
    }
  
  
  # combine all schedule sets
  completed_df <- bind_rows(sliced_df)
  
  # Count the number of weeks until 3 months from expiry date
  cutoff_dist <- assumption_tab[assumption_tab$ref == "O3", ][["value"]]
  completed_df$`Months until RP extension deadline` <- (difftime(
    completed_df$`Franchise Expiry Date` - days(cutoff_dist * 30), today(), units = c("weeks")) / 4) %>% 
    as.integer()
  # Set -ve months to 0
  completed_df$`Remaining Additional Rail Periods`[completed_df$`Months until RP extension deadline` < 0] <- 0
  
  return(completed_df)
}

# 2.0 Complete table ---------------------------------------------------------
input_df <- read_excel(filepath, sheet = "Input Sheet")
input_df$`Baseline Franchise Length (Yrs)` <- as.numeric(input_df$`Baseline Franchise Length (Yrs)`)
input_df$`Baseline Franchise Length (RPs)` <- as.numeric(input_df$`Baseline Franchise Length (RPs)`)
input_df$`Total Baseline Franchise Length (RPs)` <- as.numeric(input_df$`Total Baseline Franchise Length (RPs)`)
input_df$`Baseline Rail Period Start Number` <- as.numeric(input_df$`Baseline Rail Period Start Number`)
input_df$`Additional Rail Periods Called` <- as.numeric(input_df$`Additional Rail Periods Called`)
input_df$`Remaining Additional Rail Periods` <- as.numeric(input_df$`Remaining Additional Rail Periods`)
input_df$`Franchise Rail Period Expiry Number` <- as.numeric(input_df$`Franchise Rail Period Expiry Number`)
input_df$`Franchise Length RPS` <- as.numeric(input_df$`Franchise Length RPS`)

historic_df <- input_df %>% filter(`Franchise Expiry Date fixed` == "fixed") 
current_df <- input_df %>% filter(`Franchise Expiry Date fixed` != "fixed")
rfs_current <- complete_table(df=current_df, assumption_tab = assumption_table)
date_colnums <- func_get_date_colnums(rfs_current)

historic_df <- historic_df %>% mutate_at(date_colnums[1:(length(date_colnums))], function(x) 
  as.Date(as.POSIXct(x, format = "%Y-%m-%d", origin = "1970-01-01", tz = "UTC")))
          
rfs <- rfs_current %>% bind_rows(historic_df) %>% 
  arrange(Iteration, Franchise) 

# 3.0 Export table ---------------------------------------------------------
write.csv(rfs,paste0("data/outputs/",out_filename,".csv"))
write_rds(rfs, paste0("data/intermediates/",out_filename,".rds"))

