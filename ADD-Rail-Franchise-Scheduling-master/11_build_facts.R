# Commented out to avoid double running in the pipeline.
#library(tidyverse)
#library(readxl)
#library(DT)

# 0.0 Phase length calculation --------------------------------------------


func_phase_lengths <- function(facts_df){
  # Loop through columns, calculating the time difference between phases
  start = which(colnames(facts_df)=="Pre Project Phase")
  end = which(colnames(facts_df)=="Franchise Expiry Date")-2
  
  for(i in start:end){
    # new colname
    newname <- paste0("Phase: ",  colnames(facts_df[i]),"-", colnames(facts_df[i+1]), " (RPs)")
    #diff with previous cols in RPs
    weeks <- (facts_df[, i+1]- facts_df[, i]) /7/4 %>% as.character() %>% as.numeric()
    facts_df[, newname] <- weeks
  }
  return(facts_df)
}


# Aggregate time diff columns to find their means by competition, DA-emergency, DA-planned
func_average_phases <- function(df_phases){
  
  round_mean <- function(x){mean(x) %>% round(1)}  
  
  df_group <- df_phases %>% select(Type, contains("Phase:")) %>% 
    group_by(`Type`) %>%
    summarise_at(
      vars(`Phase: Pre Project Phase-Project Start (RPs)`:`Phase: Award-Franchise Start Date (RPs)`), 
      round_mean)

  return(df_group)
}



# 1.0 Mean phase length ---------------------------------------------------

func_phase_dt <- function(df_group){
  phase_table <- DT::datatable(df_group,
                             options = list(
                              # Layout components (Buttons, filter, r, table, i , pagination).
                              dom = "rti"
                              )
                             )%>%
  DT::formatStyle(names(df_group), fontSize = "70%")
return(phase_table)
}


# 3.0 Run code ------------------------------------------------------------
facts_df <- rfs_current %>% 
  select(-contains("fixed"), -contains("RP"), -contains("Rail"),-contains("Yrs"), - Company, -ID)

df_phases <- func_phase_lengths(facts_df)
df_aggregated <- func_average_phases(df_phases)
phase_table <- func_phase_dt(df_aggregated)



