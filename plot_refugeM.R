plot_refugeM <- function(){
  library(httr)
  library(jsonlite)
  library(ggplot2)
  
  #Store info for all refuges
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
  refDir <- data.frame(refCodes, refShorts, refNames)
  
  files <- c()
  
  #Iterate through refuges
  for(i in 1:13){
    #Make org search json
    codes <- c()
    
    if (refShorts[i] == "APB"){
      codes <- c("AP0", "APN", "APB")
    }else if (refShorts[i] == "KNI"){
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
    }else{
      ccc <- paste("FF07R", refDir$refCodes[i], "00", sep = "")
      filterRefuge <- list(list(
        order = 0,
        logicOperator = "",
        unitCode = ccc
      ))
    }
    
    #Define url and params for API request
    url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
    params <- list(
      units = filterRefuge
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
    
    files <- append(files, count)
  }
  
  #Make data frame for just refuge file counts
  refDir <- cbind(refDir, files)
  df <- data.frame(refDir$refNames, refDir$files)
  colnames(df) <- c("name","fileCount")
  
  #Make the plot
  filesbyrefuge <- ggplot(df,aes(x=name,y=fileCount,fill=name))+ 
    geom_bar(stat="identity", show.legend = FALSE) +
    labs(title = NULL, x = NULL, y = "References Added") +
    #scale_fill_manual(values = color_pal) +
    scale_fill_hue(c = 65, l = 65) +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
      panel.background = element_rect(fill = "lightblue3"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 15, face = "bold", color = "steelblue"),
      plot.margin = margin(1,1.5,1,1, "cm")
    )
  
  return(filesbyrefuge)
}

#plot_refugeM()