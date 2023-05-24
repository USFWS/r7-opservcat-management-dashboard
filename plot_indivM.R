year_by_refuge <- function(year, inputRef){
  library(httr)
  library(jsonlite)
  
  #Filter date
  filterDate <- list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "BetweenDates",
    startDate = paste(year,"-01-01",sep = ""),
    endDate = paste(year,"-12-31", sep = "")
  )
  
  #Filter people
  arlis <- c("CeliaatARLIS", "CSwansonARLIS", "stevejarlis", "mwillis", "saddison2", "lohman.lucas", "Mwjohnson2", "ErinBentley", "Valerie-ARLIS", "thodges")
  filterPeople <- list()
  
  for (k in 1:length(arlis)){
    if(k==1){
      logic <- ""
    }else{
      logic <- "OR"
    }
    filterPeople <- append(filterPeople, list(list(order = k-1, logicOperator = logic, fieldName = "Creator", searchText = arlis[k])))
  }
  
  #Filter refuge
  refCodes <- c("AM0","AP0","ARC","IZM","KAN","KNA","KU0","KDK","SWK","TET","TGK","YKD","YKF")
  refShorts <- c("AM","APB","Arc","Iz","Kan","Ken","KNI","Kod","Sel","Tet","Tog","YKD","YKF")
  refNames <- c("Alaska Maritime",
                "APB",
                "Arctic",
                "Izembek",
                "Kanuti",
                "Kenai",
                "KNI",
                "Kodiak",
                "Selawik",
                "Tetlin",
                "Togiak",
                "Yukon Delta",
                "Yukon Flats")
  
  #Get refuge index
  index <- which(refNames == inputRef)
  
  #Special cases
  codes <- c()
  if (refShorts[index] == "APB"){
    codes <- c("AP0", "APN", "APB")
  }else if (refShorts[index] == "KNI"){
    codes <- c("KU0", "KUK", "KUN", "INN")
  }
  
  if(length(codes) > 0){
    filterRefuge <- list()
    for (j in 1:length(codes)){
      ccc <- paste("FF07R", codes[j], "00", sep = "")
      if(j==1){
        logic <- ""
      }else{
        logic <- "OR"
      }
      filterRefuge <- append(filterRefuge, list(list(order = j-1, logicOperator = logic, unitCode = ccc)))
    }
  #Typical case
  }else{
    ccc <- paste("FF07R", refCodes[index], "00", sep = "")
    filterRefuge <- list(list(
      order = 0,
      logicOperator = "",
      unitCode = ccc
    ))
  }
  
  #Get First Count - TOTAL
  #Define url and params for API request
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
  params <- list(
    units = filterRefuge,
    dates = list(filterDate)
  )
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Continue if no error
  json_output <- fromJSON((content(response, as = "text")))
  total <- json_output$pageDetail$totalCount
  
  #Get Second Count - ARLIS
  #Define url and params for API request
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
  params <- list(
    units = filterRefuge,
    people = filterPeople,
    dates = list(filterDate)
  )
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Continue if no error
  json_output <- fromJSON((content(response, as = "text")))
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
    result <- year_by_refuge(x,inputRef)
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
    scale_fill_manual(values=c("gray90", "steelblue")) +
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