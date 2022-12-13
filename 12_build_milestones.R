#' Run this to plot the milestone waterfall chart and key metrics that reflects schedule performance.
#' This script reads in the pre-process data table and reshape it depnding on milestone and contract type.
#' This will output a milestone figure and a rfs_gantt table where each franchise-set date has a matching milestone name.
#'
#require(readxl)
#require(RANN)
#require(plotly)
#require(dplyr)
#require(lubridate)
#require(stringr)
#require(tidyselect)


# 1.0  Metric functions ----------------------------------------------------
func_calc_ITT_congestion_range <- function(rfs_table, neighbour_number = 7) {
  #' This function will use nearest neighbour approach to identify the densest ITT period. 
  #' It will sum the number of days to n nearest ITT dates and return the lowest value (densest region).
  #' @param rfs_table - pre-process RFS table.
  #' @param neighbour_number - Number of ITT dates to compare with
  #' @return the start and end date for the densest period.
  # Calculate most congested ITT period.
  ITT <- rfs_table$`ITT-RfP date`
  # Convert date to distance - 1990/01/01 origin.
  ITT_dist <- difftime(ITT, as.Date("1990-01-01"), units = c("days"))
  ITT_dist <- lapply(ITT_dist, function(x) x[!is.na(x)]) %>% unlist()
  neighbour_number <- if(length(ITT_dist) < neighbour_number){ITT_dist}else{neighbour_number}
  # Nearest K neighbours.
  neighbours <- nn2(ITT_dist, k = neighbour_number)
  ITT_density <- rowSums(neighbours$nn.dists)
  # Get index of minimum distance.
  min_ix <- which.min(ITT_density)
  # Get all date indices.
  neighbours$nn.idx[min_ix, ]
  # Get range.
  mx <- max(ITT[neighbours$nn.idx[min_ix, ]])
  mn <- min(ITT[neighbours$nn.idx[min_ix, ]])
  return(c(mn, mx))
}


# 1.1 Format Schedule table function ---------------------------------------
func_format_colnames <- function(rfs_table){
  #' reads in colnames from .rds file and replaces '/' and rogue names for common names
  #' @param RFS table: the complete schedule table
  #' @return RFS table with formatted colnames
  rfs_table <- rfs_table %>% rename(
    `Schedule Set` = "Iteration",
    `Pre-Proj Phase date` = "Pre Project Phase",
    `Project Start date` = "Project Start",
    `PIN date` = "PIN",
    `PQQ date` = "PQQ",
    `ITT-RfP date` = "ITT/RfP",
    `Award date` = "Award",
    `Franchise Start Date 0200 hr date` = "Franchise Start Date",
    `Franchise Length in Yrs` = "Baseline Franchise Length (Yrs)",
    `Franchise End Date 0159 hr date` = "Franchise Expiry Date"
  )
  return(rfs_table)
}

# 1.2 Milestone table function ---------------------------------------
func_shape_to_milestone_table <- function(rfs_table){
  #' Reshapes the RFS schedule to produce milestone table that can be plotted onto the milestone chart
  #' @param rfs_table: the schedule table.
  #' @return milestone table.
  fixed_cols <- names(rfs_table %>% select(contains('fixed')))
  milestone_table <- rfs_table %>%
    select(
      "Schedule Set",
      "Franchise",
      "Type",
      contains('date'), 
      -(fixed_cols)
    ) %>%
    reshape2::melt(id = c(
      "Franchise",
      "Schedule Set", 
      "Type"
    )) %>%
    mutate(variable = stringr::str_remove(variable, " date")) %>%
    rename("start_date" = value) %>%
    # Combine franchise name and iteration for labelling.
    mutate(label = paste0(Franchise, "-", `Schedule Set`)) %>%
    dplyr::arrange(label)
  # milestone_table$start_date <- as.POSIXct(as.numeric(milestone_table$start_date), origin='1970-01-01') %>% with_tz("UTC") %>% as.Date()# --------------------- NOTE- only if reads the.rds incorrectly
  return(milestone_table)
}


func_sort_franchise_set <- function(df_gantt, rfs_table) {
  #' Function that sorts ordering F1.1, F1.2, F1.3, F1.4
  #' Create `num_label` column sorts the ordering for franchise contracts.
  #' @param df_gantt: milestone table
  #' @param rfs_table: RFS table used to sort the franchises irelative to today's date
  # Create sorted index vector for the grouped labels.
  idxs <- df_gantt %>% group_by(label) %>% dplyr::group_indices()
  # NOTE: sorted franchises latest to today's date
  df_gantt$Franchise <- factor(df_gantt$Franchise, levels = rev(unique(rfs_table$Franchise[rfs_table$`Franchise Start Date 0200 hr date` > today()])))
  # Groupby Franchise
  df_gantt <- df_gantt %>%
    group_by(Franchise) %>%
    # Sort by desc iteration order
    arrange(Franchise, desc(`Schedule Set`)) %>%
    # groupby label
    group_by(label) %>%
    # reset index
    ungroup()
  # assign column to sorted indices
  df_gantt$num_label <- idxs
  return(df_gantt)
}


func_get_tick_labels <- function(df_gantt) {
  #' Function that labels ticks correctly
  #' Create `tag` column used for the y-tick labels.
  #' Only flag complete franchise-set for the first iteration.
  df_gantt <- df_gantt %>%
    # iterate row by row of df
    rowwise() %>%
    # if schedule set == 1: assign ytick = label; else: assign ytick = schedule set
    mutate(tag = ifelse(`Schedule Set` == 1, label, toString(`Schedule Set`)))
  # assign values to list
  
  return(df_gantt)
}


func_format_variable_names <- function(df_gantt){
  #' Corrects the ITT-RfP col names for FCs and DAs
  #' Will specify DA type for variable names.
  #' @param df_gantt: milestone table
  #' @return milestone table with specific name types.
  df_gantt <- df_gantt %>% 
    mutate(variable = case_when(
      `Type` %in% c("Competition", "Other") & variable == "ITT-RfP" ~ "ITT",
      `Type` %in% c("Competition", "Other") ~ variable, 
      variable == "Franchise Start Date 0200 hr" ~ paste0(`Type`," Start Date"),
      variable == "Franchise End Date 0159 hr" ~ paste0(`Type`," End Date"),
      variable == "ITT-RfP" ~ paste0(`Type`," RfP"),
      variable == "PQQ" ~ paste0(`Type`," PQQ"),
      variable == "PIN" ~ paste0(`Type`," PIN"),
      variable == "Project Start" ~ paste0(`Type`," Project Start"),
      variable == "Pre-Proj Phase" ~ paste0(`Type`," Pre-Proj Phase"),
      variable == "Award" ~ paste0(`Type`," Award"),
      TRUE ~ NA_character_)) %>%
    filter(!is.na(variable)) %>%
    select(-`Type`) 
  return(df_gantt)
}

# 1.3 Milestones chart functions -------------------------------------------
func_hline <- function(y = 0, color = "grey") {
  #' Define major axis lines
  #' Create the horizontal lines which separate each franchise on the milestone chart.
  #' Needs a y value to describe where the horizontalline should be placed on the chart.
  list(
    type = "line",
    opacity = 0.2,
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


func_legend_ordering <- function(df_gantt){
  #' Sort the rank of variables names in the milestone table so they will be displayed in a specific order.
  #' @param df_gantt: milestone table with da variable names pre-formatted.
  #' @return the df_gantt (milestone table) with ranked values in the variable column.
  df_gantt$variable <- factor(df_gantt$variable,
                              levels = c("Pre-Proj Phase", "Project Start", 
                                         "PIN","PQQ", "ITT", 
                                         "Award","Franchise Start Date 0200 hr", 
                                         "Franchise End Date 0159 hr",
                                         
                                         "DA-Planned PIN", "DA-Planned RfP",
                                         "DA-Planned Start Date", "DA-Planned End Date",
                                         "DA-Emergency Start Date", "DA-Emergency End Date"  
                              )
  )
  return(df_gantt)
}


func_milestone_plot <- function(df_gantt) {
  #' Plot the milestone for the RFS
  #' Able to filter different tasks
  # format axes
  f <- list(
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date",
    titlefont = f,
    rangeselector = list(
      buttons = list(
        list(
          count = 4,
          label = "4 months",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "1 year",
          step = "year",
          stepmode = "backward"),
        list(
          count = 10,
          label = "10 year",
          step = "year",
          stepmode = "backward"),
        list(step = "all"))) #, rangeslider = list(type = "date") # show range slider
  )
  
  y <- list(
    title = "Franchise schedule set",
    titlefont = f,
    ticktext = df_gantt$tag,
    tickvals = df_gantt$num_label,
    tickfont = list(size = 8),
    tickmode = "array"
  )
  
  # Major gridlines
  # find number of iterations per franchise
  #numb_sets <- length(unique(df_gantt$num_label)) / length(unique(df_gantt$Franchise))
  # list enumerates of labels
  #vals <- sort(unique(df_gantt$num_label))
  # create list of values
  #ymajor <- seq(0.5, last(vals) + 1, numb_sets)
  #ymajor <- ymajor[-1]
  
  #line_num <- 1
  #hlines <- list()
  #for (i in ymajor) {
  #  hlines[[line_num]] <- func_hline(i)
  #  line_num <- 1 + line_num
  #}
  
  box_labels <- ifelse(df_gantt$variable == 'PQQ','PQ',
      substr(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',df_gantt$variable,perl = TRUE),1,2)) # Get first two letters
  
  # plot milestone points
  fig <- plot_ly(df_gantt,
                 x = ~start_date,
                 y = ~num_label, # match the gantt chart y-axis
                 type = "scatter",
                 mode = "text",
                 text = box_labels,
                 sort = FALSE,
                 hovertemplate = paste(
                   df_gantt$label,
                   "<br>", df_gantt$variable, "<br>",
                   df_gantt$start_date
                 ),
                 showlegend = TRUE,
                 color = ~df_gantt$variable,
                 colors = mile_color_list,
                 marker = list(
                   size = 16,
                   symbol = "square",
                   opacity = 0.3,
                   line = list(
                     width = 1,
                     color = "#000000"
                   )
                 )
  ) %>%
    layout(title = "Milestones")
  
  
  fig <- fig %>% layout(xaxis = x, yaxis = y, #shapes = hlines, 
    font = list(
      family = "sans serif",
      size = 14
  ))
  return(fig)
}


# 2.0 Read in data -----------------------------------------------------
# Objects rfs/changed used directly from 0_data_processing

# 2.1 Format schedule table ------------------------------------------------
rfs_renamed <- func_format_colnames(rfs)
rfs_changed_renamed <- func_format_colnames(rfs_changed)%>%
  mutate_at(vars(date_colnums), as.Date, format = "%d/%m/%Y")

# 2.2 Calculate metrics -------------------------------------------------------
milestone_congestion <- rfs_renamed %>% filter(`Type` %in% c("Competition", "Other")) %>% func_calc_ITT_congestion_range()
milestone_congestion_changed <- rfs_changed_renamed %>% filter(`Type` %in% c("Competition", "Other")) %>% 
  func_calc_ITT_congestion_range()

# sort order of legend

# 2.3 Build milestone table ------------------------------------------------- NOTE- can convert into a function

milestone_table <- func_shape_to_milestone_table(rfs_renamed) %>%
  func_sort_franchise_set(rfs_renamed) %>%
  func_get_tick_labels() %>%
  func_format_variable_names() #%>%
# func_legend_ordering()

da_table <- milestone_table %>% filter(str_detect(variable, "DA"))


# 2.4 Build milestone table for changed schedule --------------------------

milestone_table_changed <- func_shape_to_milestone_table(rfs_changed_renamed) %>%
  func_sort_franchise_set(rfs_changed_renamed) %>%
  func_get_tick_labels() %>%
  func_format_variable_names() #%>%
# func_legend_ordering()

da_table_changed <- milestone_table_changed %>% filter(str_detect(variable, "DA"))



# 3.0 Build milestone chart -------------------------------------------------
milestone_fig <- func_milestone_plot(func_legend_ordering(milestone_table))
milestone_fig_changed <- func_milestone_plot(func_legend_ordering(milestone_table_changed))
