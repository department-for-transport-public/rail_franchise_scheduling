#' This runs inside the markdown to run all other data prep and outputs.
#source("tests/1_run_tests.R") # Run all tests
library(readxl)
library(tidyverse)
library(lubridate)
library(RANN)
library(plotly)
library(DT)

# 0.0 Global variables --------------------------------------------------------
filepath <<- "data/inputs/schedule_input.xlsx"
out_filename <<- "schedule_baseline"

# parameter/lookup tables
rp_lookup_table <<- read_excel(filepath, sheet = "Lookup")
assumption_table <<- read_excel(filepath, sheet = "Assumptions")
resource_assumption_table <<- read_excel(filepath, sheet = "Resource Assumptions")
# redundant milestone
remove.list <- paste(c('DA-Planned Project Start', 'DA-Planned Pre-Proj Phase', 
                       'DA-Planned Award', 'DA-Emergency Project Start', 'DA-Emergency Pre-Proj Phase', 
                       "DA-Emergency PIN", "DA-Emergency RfP", 'DA-Emergency Award'), collapse = '|')
mile_color_list <<- read_excel(filepath, sheet = "Milestone Colour Palette") %>%
  filter(!grepl(remove.list, phase_type)) %>% select(colour) %>% unlist() %>% as.character()
rm(remove.list)

forbidden_ranges <<- read_excel("data/inputs/schedule_input.xlsx", 
                                sheet = "Forbidden Dates", col_types = c("date", 
                                                                         "date", "text")) %>%
  mutate(inter = interval(`Date start`,`Date end`))



# 1.1 Run all sections of dash -----------------------------------------------------------
source("00_data_processing.R")
source("01_changes_functions.R")
source("02_changes_run.R")
rm(input_df)

source("03_export_visio_table.R")

source("10_build_datatable.R")
rm(rfs_table,fixed_colnums)

source("11_build_facts.R")
rm(df_aggregated, facts_df, df_phases)

source("12_build_milestones.R")

source("13a_build_resource_and_finance_tables.R")

source("13b_build_resource_and_finance_plots.R")
#rm(financial_year_spending, monthly_spending_table, yearly_spending_table, monthly_resource_table)
#rm(annual_EoI_profile, annual_ITT_profile, annual_PIN_profile, annual_RfP_profile)
