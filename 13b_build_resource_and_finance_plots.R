#' Part two of the resource and finance tab. Tables are pulled from part one and used to produce plots
#' of the milestone profile and adviser/resource spend profiles.

# 0.1  Plot formatting function -----------------------------------------------
funct_graph_format <- function(fig, Title = "profile", x_name = "Date", y_name = "Count") {
  #' Formats the resource/Finance and milestone profile figures.
  fig <- fig %>% layout(
    title = Title,
    paper_bgcolor = "rgb(255,255,255)", plot_bgcolor = "rgb(229,229,229)",
    xaxis = list(
      title = x_name,
      gridcolor = "rgb(255,255,255)",
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = "rgb(127,127,127)",
      ticks = "outside",
      zeroline = FALSE
    ),
    yaxis = list(
      title = y_name,
      gridcolor = "rgb(255,255,255)",
      showgrid = TRUE,
      showline = FALSE,
      showticklabels = TRUE,
      tickcolor = "rgb(127,127,127)",
      ticks = "outside",
      zeroline = FALSE
    )
  )
  return(fig)
}

# 0.2  Adviser/resource plots -----------------------------------------------
funct_simple_fig <- function(monthly_spending_table, title_text, y_text){
  #' Plot line graph of monthly resource and finance profiles
  #' @param monthly_spending_table: the first table you want to plot a line of
  #' @param title_text title of chart
  #' @param y_text x-axis label
  fig <- plot_ly(monthly_spending_table, x = ~calendar_month, 
                 y = ~Total, type = "scatter", mode = "lines+markers", 
                 name="Baseline", opacity = 0.6, line = list(shape = "spline"))
  fig <- funct_graph_format(fig, Title = title_text, x_name = "Date", y_name = y_text)
  return(fig)
}


funct_year_fig <- function(yearly_spending_table, title, y_text){
  df_year_fig <- plot_ly(yearly_spending_table, x = ~Finyear, y = ~Total, type = "scatter", name = "baseline", mode = "lines", line = list(shape = "spline"))
  df_year_fig <- df_year_fig %>% add_trace(yearly_spending_table, x = ~Finyear, name = "baseline", y = ~Total, type = "bar")
  df_year_fig <- funct_graph_format(df_year_fig, Title = title , x_name = "Date", y_name = y_text)
  return(df_year_fig)
}


# 0.3  Milestone profile plot -----------------------------------------------
plot_milestone_profile <- function(df_profile, milestone_type ){
  fig <- plot_ly(df_profile, x = ~Year, y = ~n, type = "bar")
  fig <- funct_graph_format(fig, Title = paste0(milestone_type," Profile in financial years"), x_name = "Year", y_name = paste0("No. of ", milestone_type))
  return(fig)
}

# 1.0  Multiple trace plots -----------------------------------------------
# 1.1  Combine user and baseline tables -----------------------------------------------
funct_combine_user_baseline_table <- function(df_user, df_base, period ="calendar_month"){
  #' joins the user changes and baseline count table together so they will have the same number of datapoints and therefore can be plotted.
  #' @param df_user: the user changes count table
  #' @param df_base:the baseline count table
  #' @param period: check if it's 'year' or 'month' 
  #' @return combined table with calendar month/year, Total.x (= baseline total), Total.y (= user-changes total)
  # collect relevant cols
  if (period == 'calendar_month'){
    df_base <- df_base %>% select(calendar_month, Total)
    df_user <- df_user %>% select(calendar_month, Total)
  }else{
    df_base <- df_base %>% select(Finyear, Total)
    df_user <- df_user %>% select(Finyear, Total)
  }
  
  # join baseline and changes table
  comb_tbl <- left_join(df_base, df_user, by = period)
  return(comb_tbl)
}

# 1.2  Monthly plot -----------------------------------------------
plot_monthly_profiles <- function(df_combined, title_text, y_text){
  #' Uses the combined df to produce a monthly profile plot of user changes and baseline
  #' @param df_combined: monthly profile with total user chages and baseline
  #' @param title_text: graph title
  #' @param y_text: y-axis label
  #' @return figures of both traces
  
  # combined figure plot
  fig <- df_combined %>% plot_ly(x = ~calendar_month, 
                                 y = ~Total.x, type = "scatter", mode = 'lines+markers', 
                                 name="Baseline", opacity = 0.6, line = list(shape = "spline"), marker = list(size = 3.5)) %>%
    add_trace(x = ~calendar_month, 
              y = ~Total.y, type = "scatter", name="User changes", 
              mode = 'lines+markers', opacity = 0.6, line = list(shape = "spline"), marker = list(size = 3.5))
  fig <- funct_graph_format(fig, Title = title_text, x_name = "Date", y_name = y_text)
  return(fig)
}

# 1.3  Annual plots -----------------------------------------------
plot_yearly_profiles <- function(df_combined, title_text, y_text){
  #' Uses the combined df to produce a yearly profile plot of user changes and baseline
  #' @param df_combined: yearly profile with total user chages and baseline
  #' @param title_text: graph title
  #' @param y_text: y-axis label
  #' @return figures of both traces
  
  # combined figure plot
  fig <- df_combined %>% plot_ly(x = ~Finyear, 
                                 y = ~Total.x, type = "scatter", mode = "lines", 
                                 name="Baseline", opacity = 0.6, line = list(shape = "spline")) %>%
    add_trace(x = ~Finyear, y = ~Total.x, name = "baseline", type = "bar") %>%
    add_trace(x = ~Finyear, 
              y = ~Total.y, type = "scatter", name="User changes", 
              mode = "lines", opacity = 0.6, line = list(shape = "spline"))
  fig <- funct_graph_format(fig, Title = title_text, x_name = "Date", y_name = y_text)
  return(fig)
}

# 2.0  Main -----------------------------------------------
# plot monthly resource and finance charts
resource_fig <<- funct_simple_fig(monthly_resource_table, "Full Time Equivalent Resourcing Profile", "Resource total (No. workers)")
spending_fig <<- funct_simple_fig(monthly_spending_table, "Adviser Spending Profile", "Spending total")

# plot yearly resource and finance charts
fin_year_fig <<- yearly_spending_table %>%
  funct_year_fig(title =  "Financial Year Spending Profile", y_text = "Annual spend")
res_year_fig <<- yearly_resource_table %>%
  funct_year_fig(title =  "Financial Year Full Time Equivalent Resourcing Profile", y_text = "No. of workers")


# Competition profiles
ITT_table <-funct_norm_timescale(milestone_table, milestone_type = "ITT")
ITT_fig <<- ITT_table  %>%
  plot_milestone_profile(milestone_type = "ITT")

PQQ_table <- funct_norm_timescale (milestone_table, milestone_type = "PQQ")
PQQ_fig <<- PQQ_table %>%
  plot_milestone_profile(milestone_type = "PQQ")

# Direct Award profiles
PIN_table <- funct_norm_timescale (milestone_table, milestone_type = "DA-Planned PIN")
PIN_fig <<- PIN_table %>%
  plot_milestone_profile(milestone_type = "PIN")

RfP_table <- funct_norm_timescale (milestone_table, milestone_type = "RfP")
RfP_fig <<- RfP_table %>%
  plot_milestone_profile(milestone_type = "RfP")

# Plot comparative monthly profile
spending_fig <<- funct_combine_user_baseline_table(changed_monthly_spending_table, monthly_spending_table) %>%
  plot_monthly_profiles("Adviser Spending Profile", "Spending total")

resource_fig <<- funct_combine_user_baseline_table(changed_monthly_resource_table, monthly_resource_table) %>%
  plot_monthly_profiles("Full Time Equivalent Resourcing Profile", "Resource total (No. workers)")

# Plot comparative yearly profile
fin_year_fig <<- funct_combine_user_baseline_table(changed_yearly_spending_table, yearly_spending_table, "Finyear") %>%
  plot_yearly_profiles("Financial Year Spending Profile", "Annual spend")

res_year_fig <<- funct_combine_user_baseline_table(changed_yearly_resource_table, yearly_resource_table, "Finyear") %>%
  plot_yearly_profiles("Financial Year Full Time Equivalent Resourcing Profile", "Resource total (No. workers)")
#}

# Export milestone profiles
write.csv(ITT_table, "data/outputs/annual_mile_ITT.csv", row.names=FALSE)
write.csv(PQQ_table,"data/outputs/annual_mile_PQQ.csv", row.names=FALSE)
write.csv(PIN_table,"data/outputs/annual_mile_PIN.csv", row.names=FALSE)
write.csv(RfP_table,"data/outputs/annual_mile_RfP.csv", row.names=FALSE)


# Clean workspace
rm(monthly_spending_table, monthly_resource_table, yearly_spending_table,
   yearly_resource_table, changed_monthly_spending_table, changed_monthly_resource_table, changed_yearly_spending_table,
   changed_yearly_resource_table, ITT_table, PQQ_table, PIN_table, RfP_table)
