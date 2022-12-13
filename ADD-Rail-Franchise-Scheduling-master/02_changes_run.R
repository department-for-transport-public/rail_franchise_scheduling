#' This script iterates through the user changes input tab and calls functions from `01_changes_funtions.R` to
#' implements in to the RFS table. Changes that cannot be processed are logged into the `log.txt` sheet.
#' To clean log file either: manually delete data/log.txt file or run following line below:
# write("",file="data/log.txt", append=FALSE)

# This section is commented out to avoid double running in the pipeline.
# library(tidyverse)
# library(readxl)
# library(lubridate)
# source("00_data_processing.R")
# source("01_changes_functions.R")

#  Altered assumptions table ----------------------------------------------
# Bring in altered assumption table. Build new table using this.
date_colnums_chng <- func_get_date_colnums(input_df)
alt_assumptions_table <- read_excel(filepath, sheet = "Assumptions - Alt")
rfs_changed <- complete_table(current_df, assumption_tab = alt_assumptions_table) %>%
  mutate_at(date_colnums_chng, as.Date)


# Update all changes ------------------------------------------------------

# read in user changes tab with pre-defined column types
changes <- read_excel(filepath,
  sheet = "Changes", col_types = c(
    "text",
    "numeric", "text", "text", "numeric"
  )
) %>%
  # create label column as an ID and convert datetime to date
  mutate(
    ID = paste0(Franchise, "-", Iteration)
  )

# iterate through each user change, trying to implement
# and catching any errors to be logged in file
sapply(seq(1, nrow(changes), 1), function(x) {
  tryCatch(
    {
      run_each(change = changes[x, ], dfc = rfs_changed, assumption_tab = alt_assumptions_table)
      cat(paste0("\nImplemented user change ", x, "\n"))
    },
    error = function(e) {
      cat(paste0("Ignored user change: ", x, "\n", e))
      write(paste0("Date: ", today(), " User change: ", x, "\n", e), file = "data/log.txt", append = TRUE)
    },
    finally = {
      cat("\n")
    }
  )
})

rfs_current_changed = rfs_changed
rfs_changed <- rfs_changed %>% mutate_at(date_colnums, as.Date) %>% bind_rows(historic_df) %>% 
  arrange(Iteration, Franchise)

# export user changed RFS table to RDS and CSV
write.csv(rfs_changed, paste0("data/outputs/", out_filename, "_changed.csv"))
write_rds(rfs_changed, paste0("data/outputs/", out_filename, "_changed.rds"))
