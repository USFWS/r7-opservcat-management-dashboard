program_totals_by_year <- function(year){
  library(httr)
  library(jsonlite)

  params <- list(
    units = query_orgs(return_refprogram_list()),
    dates = query_year(year)
  )
  json_output <- api_call(params)
  count <- json_output$pageDetail$totalCount
  
  return(count)
}

plot_yearsM <- function(){
  library(httr)
  library(jsonlite)
  
  years <- c()
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  for (i in 2011:currentYear) {
    years <- append(years, i)
  }
  
  yearCounts <- c()
  for(x in years){
    yearCounts <- append(yearCounts, program_totals_by_year(x))
  }
  
  #Make graph
  df <- data.frame(years, yearCounts)
  library(ggplot2)
  yearPlot <- ggplot(df,aes(x=years,y=yearCounts))+ 
    geom_bar(stat="identity", fill = "steelblue4") +
    scale_x_continuous(breaks = df$years, labels = df$years) +
    labs(title = NULL, x = NULL, y = "References Added") +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      panel.background = element_rect(fill = "lightblue3"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(color = "steelblue", size = 15, face = "bold"),
      plot.margin = margin(1,1.5,1,1, "cm")
    )
  return(yearPlot)
}

#Alternative plot: line graph witn cumulative totals
plot_years_cumulativeM <- function(){
  years <- c()
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  for (i in 2011:currentYear) {
    years <- append(years, i)
  }
  
  yearCounts <- c()
  prev <- 0
  for(x in years){
    yearCounts <- append(yearCounts, program_totals_by_year(x) + prev)
    prev <- yearCounts[length(yearCounts)]
  }
  
  #Make graph
  df <- data.frame(years, yearCounts)
  library(ggplot2)
  yearPlot <- ggplot(df,aes(x=years,y=yearCounts))+ 
    geom_line(color = "steelblue4") +
    geom_point(color = "steelblue4") +
    scale_x_continuous(breaks = df$years, labels = df$years) +
    labs(title = NULL, x = NULL, y = "Total References in ServCat") +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      panel.background = element_rect(fill = "lightblue3"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(color = "steelblue", size = 15, face = "bold"),
      plot.margin = margin(1,1.5,1,1, "cm")
    )
  return(yearPlot)
}