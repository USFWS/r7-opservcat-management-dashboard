refuge_totals_by_year <- function(year, inputRef){
  library(httr)
  library(jsonlite)
  
  #Get First Count - TOTAL
  params <- list(
    units = query_refuge(inputRef),
    dates = query_year(year)
  )
  json_output <- api_call(params)
  total <- json_output$pageDetail$totalCount
  
  #Get Second Count - ARLIS
  params <- list(
    units = query_refuge(inputRef),
    people = query_ARLIScreators(),
    dates = query_year(year)
  )
  json_output <- api_call(params)
  arlis <- json_output$pageDetail$totalCount
  
  #Get counts of interest
  other <- total-arlis
  
  bothcounts <- c(other, arlis)
  return(bothcounts)
}




#SECOND FUNCTION
plot_indivM <- function(inputRef){
  library(httr)
  library(jsonlite)
  
  years <- c()
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  for (i in 2011:currentYear) {
    years <- append(years, i)
  }
  
  countOther <- c()
  countArlis <- c()
  for(x in years){
    result <- refuge_totals_by_year(x,inputRef)
    countOther <- append(countOther, result[1])
    countArlis <- append(countArlis, result[2])
  }
  
  #Doubled dataframe
  types <- append(rep("Added by ARLIS", length(years)), rep("Other", length(years)))
  years <- append(years, years)
  counts <- append(countArlis, countOther)
  
  #Make graph
  df <- data.frame(years, types, counts)
  library(ggplot2)
  yearPlot <- ggplot(df,aes(x=years,y=counts, fill=factor(types, levels=c("Other", "Added by ARLIS"))))+ 
    geom_bar(stat="identity", position="stack") +
    scale_x_continuous(breaks = df$years, labels = df$years) +
    labs(title = NULL, x = NULL, y = "References Added") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values=c("mediumseagreen", "steelblue")) +
    #annotate("text",  x=Inf, y = Inf, label = "Phase 1", vjust=1, hjust=0.5) +
    #scale_y_continuous(limits = c(0,NA)) +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      panel.background = element_rect(fill = "lightblue3"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(color = "steelblue", size = 15, face = "bold"),
      plot.margin = margin(1,1,1,1, "cm"),
      legend.title=element_blank()
    )
  
  build <- ggplot_build(yearPlot)
  ymax <- build$layout$panel_params[[1]]$y.range[2]
  mark <- 0.85*ymax
  
  yearPlot <- yearPlot +
    geom_text(aes(x=2013.5, label="\nPhase 1", y=mark), colour="steelblue4", angle=90, size=4) +
    geom_vline(xintercept = 2013.5, linetype="longdash", color = "steelblue4", size=0.5) +
    geom_text(aes(x=2019.5, label="\nPhase 2", y=mark), colour="steelblue4", angle=90, size=4) +
    geom_vline(xintercept = 2019.5, linetype="longdash", color = "steelblue4", size=0.5) +
    geom_text(aes(x=2022.5, label="\nPhase 3", y=mark), colour="steelblue4", angle=90, size=4) +
    geom_vline(xintercept = 2022.5, linetype="longdash", color = "steelblue4", size=0.5)
  return(yearPlot)
}

#plot_indivM("Togiak")