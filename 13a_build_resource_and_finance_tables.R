#' Calculates the cost of resources and expenses to run each phase for every calendar month/year using the RFS table.
#' Returns these spending profiles as line/bar figures
#' Also return key annual milestone profile figures

# 0.1  Monthly contract phase table -----------------------------------------------
funct_count_table <- function(calendar_month, rfs_tabl){
  #' For each phase, this function iterates through each calendar month and tallies the phase. This is done for Franchise Competiton (fc) and Direct Award (da).
  #' @param calendar_month A monthly calendar table starting from the first phase start date in the RFS table and ends at the last start date.
  #' @param rfs_tabl: rfs table where phases are binned up by months
  #' @return a monthly table counting the number of each phase in a given month
  count_table <- tibble(calendar_month)
  
  # Count FC phases for each month
  # count the number of RFS rows that are not DA and the calendar month, x, sits between the RFS pre-proj phase date and RFS project start date
  count_table$fc_pre_proj_to_proj_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Pre Project Phase` <= x & x < rfs_tabl$`Project Start` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  count_table$fc_proj_start_to_PIN <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Project Start` <= x & x < rfs_tabl$`PIN` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  count_table$fc_PIN_to_PQQ <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`PIN` <= x & x < rfs_tabl$`PQQ` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  count_table$fc_PQQ_to_ITT <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`PQQ` <= x & x < rfs_tabl$`ITT/RfP` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  count_table$fc_ITT_to_award <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`ITT/RfP` <= x & x < rfs_tabl$`Award` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  count_table$fc_award_to_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Award` <= x & x < rfs_tabl$`Franchise Start Date` & rfs_tabl$`Type` %in% c("Competition", "Other"))))
  
  # Count Planned DA phases for each month
  # count the number of RFS rows that are DA and the calendar month, x, sits between the RFS pre-proj phase date and RFS project start date
  count_table$da_pre_proj_to_proj_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Pre Project Phase` <= x & x < rfs_tabl$`Project Start` & rfs_tabl$`Type` == "DA-Planned")))
  count_table$da_proj_start_to_PIN <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Project Start` <= x & x < rfs_tabl$`PIN` & rfs_tabl$`Type` == "DA-Planned")))
  count_table$da_PIN_to_RfP <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`PIN` <= x & x < rfs_tabl$`ITT/RfP` & rfs_tabl$`Type` == "DA-Planned")))
  count_table$da_RfP_to_award <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`ITT/RfP` <= x & x < rfs_tabl$`Award` & rfs_tabl$`Type` == "DA-Planned")))
  count_table$da_award_to_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Award` <= x & x < rfs_tabl$`Franchise Start Date` & rfs_tabl$`Type` == "DA-Planned")))
  
  # Count Emergency DA phases for each month	
  # count the number of RFS rows that are DA and the calendar month, x, sits between the RFS pre-proj phase date and RFS project start date	
  count_table$eda_pre_proj_to_proj_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Pre Project Phase` <= x & x < rfs_tabl$`Project Start` & rfs_tabl$`Type` == "DA-Emergency")))	
  count_table$eda_proj_start_to_RfP <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Project Start` <= x & x < rfs_tabl$`ITT/RfP` & rfs_tabl$`Type` == "DA-Emergency")))	
  
  count_table$eda_RfP_to_award <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`ITT/RfP` <= x & x < rfs_tabl$`Award` & rfs_tabl$`Type` == "DA-Emergency")))	
  count_table$eda_award_to_start <- unlist(lapply(count_table$calendar_month, function(x) sum(rfs_tabl$`Award` <= x & x < rfs_tabl$`Franchise Start Date` & rfs_tabl$`Type` == "DA-Emergency")))
  
  return(count_table)
}


# 0.2  Pre-process resource assumption table -----------------------------------------------
funct_spending_table <- function(assumption_df){
  #' Reshape the resource assumption table to match the format of the RFS table.
  #' @param assumption_df resource assumptions from input sheet
  #' @return assumption_df
  
  # Index table by project phase
  rownames(assumption_df) <- assumption_df$`Project Phase`
  # Transpose table
  spending_table <- assumption_df %>%
    mutate(`Total` = `Legal expenses` + `Finance expenses` + `Technical expenses`)%>%
    select(`Total`, `Staff required`) %>%
    t()
  return(spending_table)
}


# 0.3  Aggregated monthly/annual resource table -----------------------------------------------
funct_monthly_aggregation <- function(count_table, calendar_month=rfs_month_list, assumption_df=spending_table, spending_table_column="Total"){
  #' 
  #' @param count_table: the table which contains calendar months and tallied phase count
  #' @param assumption_df: the transposed reshaped resource cost table
  #' @param spending_table_column: the name of the resource column you want to multiply by.
  #' @return the returns the total cost of resource/finance of each phase for each month.
  # get column names	
  phase_names <- as_tibble(spending_table) %>% mutate(name = row.names(spending_table)) %>% 
    filter(name == 'Total') %>% select(-name) %>% colnames()
  
  # Test 3rd value of fc_pre_proj_to_proj_start should be (2 * 77758.33) = 155516.67, not 749641.6 = (2 * 374820.8)	  monthly_spending_table <- count_table[, 2:11] * assumption_df[spending_table_column, ]
  monthly_spending_table <- as_tibble(t(as_tibble(t(count_table[, 2:ncol(count_table)]) * t(assumption_df[spending_table_column, ]) %>% as.vector())))	
  monthly_spending_table <- setNames(monthly_spending_table, phase_names)
  
  # Calculate total monthly spending
  monthly_spending_table$`Total` <- rowSums(monthly_spending_table)
  # bind monthly column back to cost table
  monthly_spending_table <- cbind(calendar_month, monthly_spending_table)
  return(monthly_spending_table)
}


funct_resource_year <- function(monthly_spending_table){
  #' aggregate the monthly costs by the financal year
  #' @param monthly_spending_table: spending aggregate by month
  #' @return spending table aggregated by financial year.
  yearly_spending_table <- monthly_spending_table %>%
    mutate(
      Year = year(calendar_month),
      Quarter = quarter(calendar_month),
      Finyear = ifelse(Quarter < 2, Year, Year + 1)
    ) %>%
    group_by(Finyear) %>%
    summarise(Total = sum(Total))
  return(yearly_spending_table)
}


# 0.4  Annual milestone profile table -----------------------------------------------
funct_norm_timescale <- function(milestone_tabl, milestone_type){
  #' produces an annual milestone profile table which can be plotted and have consistent time range.
  #' @param milestone_tabl: the milestone table used to find the minimum/maximum start date
  #' @param milestone_type: the milestone name that want to profile
  #' @return milestone profile table that can be plotted with consistent date range.
  
  # create profile table
  profile <- milestone_tabl %>%
    # filter required milestone
    filter(str_detect(variable, milestone_type)) %>%
    # aggregate the profile by year
    mutate(Year = year(start_date)) %>%
    group_by(Year) %>%
    count()
  # add zero value for the max and min start date in the data set so all profiles have the same date range
  profile[nrow(profile) + 1, ] <- list( min(year(milestone_tabl$start_date)), 0)
  profile[nrow(profile) + 1, ] <- list( max(year(milestone_tabl$start_date)), 0)
  return(profile)  
}

# 1.0  Main -----------------------------------------------

# Do not handle incomplete historic details
milestone_table_current <- milestone_table %>% inner_join(rfs_current, by=c("label" = "ID"))
milestone_table_current_changed <- milestone_table_changed %>% inner_join(rfs_current_changed, by=c("label" = "ID"))


# List of calendar months of range
rfs_month_list <- seq(min((milestone_table_current %>% filter(!is.na(start_date)))$start_date), 
                      max((milestone_table_current %>% filter(!is.na(start_date)))$start_date), 
                      by = "month") # create monthly calendar in range of RFS table

count_table <- funct_count_table(rfs_month_list, rfs_tabl = rfs_current)
spending_table <- funct_spending_table(resource_assumption_table)

# Build aggregated tables
monthly_spending_table <<- funct_monthly_aggregation(count_table, rfs_month_list, spending_table, "Total")
monthly_resource_table <<- funct_monthly_aggregation(count_table, rfs_month_list, spending_table, "Staff required")
yearly_spending_table <<-  funct_resource_year(monthly_spending_table)
yearly_resource_table <<- funct_resource_year(monthly_resource_table)

# Key metrics
annual_ITT_profile <- funct_norm_timescale (milestone_table_current, milestone_type = "ITT")
more_than_2IIT <<- sum(annual_ITT_profile$n >2)
first_year_more_2ITT <<- annual_ITT_profile[annual_ITT_profile$n >2,]$Year

# Build user changed tables
count_table_mod <- funct_count_table(rfs_month_list, rfs_tabl = rfs_current_changed)
changed_monthly_spending_table <<- funct_monthly_aggregation(count_table_mod, rfs_month_list, spending_table, "Total")
changed_monthly_resource_table <<- funct_monthly_aggregation(count_table_mod, rfs_month_list, spending_table, "Staff required")
changed_yearly_spending_table <<- funct_resource_year(changed_monthly_spending_table)
changed_yearly_resource_table <<- funct_resource_year(changed_monthly_resource_table)

# Key metrics
annual_ITT_profile <- funct_norm_timescale (milestone_table_current_changed, milestone_type = "ITT")
more_than_2IIT_mod <<- sum(annual_ITT_profile$n >2)
first_year_more_2ITT_mod <<- annual_ITT_profile[annual_ITT_profile$n >2,]$Year

# Export datatables to csv
write.csv(monthly_spending_table, "data/outputs/monthly_adviser.csv", row.names=FALSE)
write.csv(yearly_spending_table, "data/outputs/annual_adviser.csv", row.names=FALSE)
write.csv(monthly_resource_table, "data/outputs/monthly_resource.csv", row.names=FALSE)
write.csv(yearly_resource_table, "data/outputs/annual_resource.csv", row.names=FALSE)

# Clean workspace
rm(rfs_month_list, count_table, count_table_mod, spending_table)
