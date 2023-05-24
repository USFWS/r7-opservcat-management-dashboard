years_api_call <- function(year){
  library(httr)
  library(jsonlite)

  refCCC <- c("AM0","AP0","APN","APB","ARC","IZM","KAN","KNA","KU0","KUK","KUN","INN","KDK","SWK","TET","TGK","YKD","YKF")
  otherCCC <- c("000","010","020","02M","02W","030","040","050","060","080","090","091","092","AMT","0MD","YFY")
  allCCC <- append(refCCC, otherCCC)
  filterRefuge <- list()
  for (x in 1:length(allCCC)){
    if(x==1){
      logic <- ""
    }else{
      logic <- "OR"
    }
    ccc <- paste("FF07R", allCCC[x], "00", sep = "")
    filterRefuge <- append(filterRefuge, list(list(order = x-1, logicOperator = logic, unitCode = ccc)))
  }
  
  filterDate <- list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "BetweenDates",
    startDate = paste(year,"-01-01",sep = ""),
    endDate = paste(year,"-12-31",sep = "")
  )
  
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
    yearCounts <- append(yearCounts, years_api_call(x))
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

#plot_yearsM()