# This section is commented out to avoid double running in the pipeline.
# library(tidyverse)
# library(readxl)
# library(lubridate)
# source("00_data_processing.R")

# Functions ---------------------------------------------------------------
# Common ------------------------------------------------------------------

# Which row in schedule matches the change we want to make
find_matching_row <- function(id_name, dfc) {
  match_row <- which(dfc$ID == id_name)
  return(match_row)
}

# Switch out old row for new changed row.
replace_old_new <- function(new_row, dfc) {
  new_df <- dfc
  matching_row <- find_matching_row(new_row$ID, dfc)
  new_df[matching_row, ] <- new_row
  return(new_df)
}

# Function that returns list of the names of 'fixed' columns
locate_fixed_cols <- function(dfc) {
  colnums <- names(dfc %>% select(contains("fixed")))
  return(colnums)
}

get_previous_iteration_change <- function(change){
  new_change <- change %>% mutate(
    `Iteration` = `Iteration` - 1,
    `Change type` = 'Contract extension',
    `Change subtype` = "Extension",
    `ID` = paste0(`Franchise`, "-", `Iteration`)
  )
  return(new_change)
  
}


# Add new DA --------------------------------------------------------------

new_da_row <- function(change, dfc) {
  # Add a new DA to the schedule.
  # Build the new row by stripping the old one (need to keep data types)
  # and replacing the minimum number of relevant fields.
  
  # sort by ID e.g. F1-1, F1-2,..., F2-1, F2-2
  dfc <- dfc %>% arrange(`ID`)
  matching_row <- find_matching_row(change$ID, dfc)
  
  # condition - prevent changes to fixed start date
  if (isTRUE(dfc[matching_row, ]$`Franchise Start Date fixed` == "fixed")) {
    stop("A DA cannot be used here as the franchise start date has been fixed, try the next iteration.")
  }
  else {
    # not needed as it's called from the 0_data_processing.R as `date_colnums`
    fixed_colnums <- which(str_detect(names(dfc), "fixed"))
    
    new_row <- dfc[matching_row, ] %>%
      # fill fixed cols as NAN
      mutate_at(fixed_colnums, ~"changed") %>%
      # fill date cols as NAN
      mutate_at(date_colnums, ~NA_character_) %>%
      # fill date cols as NAN
      mutate(
        Company = NA_character_,
        `Iteration` = `Iteration`,
        `Franchise Expiry Date` = NA_character_,
        `Total Baseline Franchise Length (RPs)` = NA,
        `Type` = change$`Change subtype`,
        `Additional Rail Periods Called` = 0,
        `Remaining Additional Rail Periods` = 0,
        `Baseline Franchise Length (Yrs)` = change$Change %/% 13,
        `Baseline Franchise Length (RPs)` = change$Change %% 13,
        `Months until RP extension deadline` = NA,
        `Franchise Rail Period Expiry Number` = NA
      ) %>%
      mutate_at(date_colnums, as.Date)
    
    print("DA start date is taken as the start date of the franchise it is replacing")
    # get start date
    new_row <- new_row %>% mutate(`Franchise Start Date` = dfc[matching_row, ]$`Franchise Start Date`)
    
    return(new_row)
  }
}

# Add extension to existing contract --------------------------------------

extend_RP <- function(dfc, change) {
  #' add rail period extensions to an existing contract
  #' @param dfc: the rfs table that will be changed
  #' @param change: a row from the changes sheet
  #' @return changed rfs table with an extended contract
  # Add rail periods onto existing contract and recompute the knockons.
  matching_row <- find_matching_row(change$ID, dfc)
  
  # condition - enough remaining RP to make change
  if (change$Change > dfc[matching_row, "Remaining Additional Rail Periods"]) {
    stop("There are not enough rail periods available for this change, please reduce the number or add a DA.")
  } 
  # condition - do not move 'fixed' expiry dates
  else if (!is.na(dfc[matching_row, "Franchise Expiry Date fixed"]) & dfc[matching_row, "Franchise Expiry Date fixed"] == "fixed") {
    stop("You cannot overwrite a fixed expiry date.")
  }
  
  else {
    new_row <- dfc[matching_row, ] %>% mutate(
      # condtion - only one extension can be called
      `Remaining Additional Rail Periods` = 0,
      # update # rail periods called
      `Additional Rail Periods Called` = `Additional Rail Periods Called` + change$Change,
      # relabel fix status to extended
      `Franchise Expiry Date fixed` = "extended"
    )
    return(new_row)
  }
}


mileextend_RP <- function(dfc, change) {
  #' add rail period extensions to an existing contract
  #' @param dfc: the rfs table that will be changed
  #' @param change: a row from the changes sheet
  #' @return changed rfs table with an extended contract
  # Add rail periods onto existing contract and recompute the knockons.
  matching_row <- find_matching_row(change$ID, dfc)
  
  # condition - enough remaining RP to make change
  if (change$Change > dfc[matching_row-1, "Remaining Additional Rail Periods"]) {
    stop("There are not enough rail periods available for this change, please reduce the number or add a DA.")
  } 
  # condition - do not move 'fixed' expiry dates
  else if (!is.na(dfc[matching_row-1, "Franchise Expiry Date fixed"]) & dfc[matching_row-1, "Franchise Expiry Date fixed"] == "fixed") {
    stop("You cannot overwrite a fixed expiry date.")
  }
  
  else {
    new_row <- dfc[matching_row-1, ] %>% mutate(
      # condtion - only one extension can be called
      `Remaining Additional Rail Periods` = 0,
      # update # rail periods called
      `Additional Rail Periods Called` = `Additional Rail Periods Called` + change$Change,
      # relabel fix status to extended
      `Franchise Expiry Date fixed` = "extended"
    )
    return(new_row)
  }
}

change_contract_length <- function(dfc, change) {
  #' Change the baseline contract length in years for a specific franchise.
  #' @param dfc: the rfs table that will be changed
  #' @param change: a row from the changes sheet
  #' @return changed rfs table with a new baseline contract length
  
  # condition - prevent fixed dates from changing
  matching_row <- find_matching_row(change$ID, dfc)
  
  if (!is.na(dfc[matching_row, "Franchise Expiry Date fixed"]) & dfc[matching_row, "Franchise Expiry Date fixed"] == "fixed") {
    stop("You cannot overwrite a fixed expiry date.")
  }
  else {
    new_row <- dfc[matching_row,] %>% mutate(
      # change the baseline fracnhise years using the absolute contract length in changes
      `Baseline Franchise Length (Yrs)` = change$Change,
      # relabel fix status to 'changed'
      `Franchise Expiry Date fixed` = "changed"
    )
    return(new_row)
  }
}

# Run for each function ---------------------------------------------------

run_each <- function(change, dfc, assumption_tab) {
  #' Three kinds of user changes:
  #' 1. Extension - using additional remaining rp to extend franchise contract
  #' 2. Add DA - Add a DA between two existing contracts to fill space between handover
  #' 3. Change milestone date - Change the milestone of a specific date.
  #'
  # sort df by ID
  dfc <- dfc %>% arrange(ID)
  # locate row in RFS table that needs changing
  matching_row <- find_matching_row(change$ID, dfc)
  
  # DA issue
  if (change$`Change type` == "Add DA") {
    # condition - valid change input
    if (is.na(change$Change)) {
      stop("DA's require a length in rail periods.")
    }
    else {
      new_df <- replace_old_new(new_da_row(change, dfc), dfc)
      new_complete <- complete_table(new_df, assumption_tab = alt_assumptions_table)
      rfs_changed <<- new_complete
    }
    
  } else if (change$`Change type` == "Extension" | change$`Change type` == "Contract length change") {
    # contract extension and contract length
    if (is.na(change$Change)) {
      stop("Contracts/extensions changes need to be supplied with a length in rail periods.")
    } else if (change$`Change type` == "Extension") {
      # add extension to contract
      new_df <- replace_old_new(extend_RP(dfc, change), dfc)
      rfs_changed <<- complete_table(new_df, alt_assumptions_table)
      
    } else if (change$`Change type` == "Contract length change") {
      new_df <- replace_old_new(change_contract_length(dfc, change), dfc)
      new_complete <- complete_table(new_df, alt_assumptions_table)
      rfs_changed <<- new_complete
    }
  } else if (change$`Change type` == "Milestone change") {
    # Change start date milestone date
    # condition - check user changes filled correctly
    if (is.na(change$Change)) {
      stop("Franchise start date changes need to be supplied with a shift in rail periods.")
    
    } else {
      
      min_iteration <- dfc %>% filter(Franchise == change$Franchise) %>% select(Iteration) %>% min()
      if(change$Iteration > min_iteration){
        # Condition when non-first line iteration is being changed.
        # In this case we alter the line of the previous contract's end date and check it has enough
        # rail periods to support this.
        new_df <- replace_old_new(mileextend_RP(dfc, change), dfc)
        
      } else{
        # Condition when it is the first iteration. In this case we only deal with the line in question.
        new_row <- dfc[matching_row,]
        
        if (!is.na(new_row$`Franchise Start Date fixed`) & new_row$`Franchise Start Date fixed` == "fixed"){
          stop("You cannot overwrite a fixed expiry date.")
          
        } else {
          # Build new row by calculating new start and end numbers and dates.
          new_row <- new_row %>% 
            mutate(`Baseline Rail Period Start Number` = `Baseline Rail Period Start Number` + change$Change,
                   `Franchise Expiry Date fixed` = is.Date(NA)) %>% 
            left_join(rp_lookup_table, by = c("Baseline Rail Period Start Number"="Rail Period Number")) %>%
            mutate(`Franchise Start Date` = RP) %>%
            mutate(`Franchise Expiry Date fixed` = "changed") %>%
            select(-RP, -`Rail Period`)
          new_df <- replace_old_new(new_row,dfc)
        }
      }
      
      rfs_changed <<- complete_table(new_df, alt_assumptions_table)
    }
  }
  
  # other
  else {
    (stop("The change type you have selected is not recognised."))
  }
}
