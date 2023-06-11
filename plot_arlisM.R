plot_arlisM <- function(){
  library(httr)
  library(jsonlite)
  library(ggplot2)
  
  codesdf <- return_refuge_df()
  files <- c()
  
  #Iterate through refuges to get file count for each
  for (i in 1:length(codesdf$names)){
    params <- list(
      units = query_refuge(codesdf$names[i]),
      people = query_ARLIScreators()
    )
    
    json_output <- api_call(params)
    count <- json_output$pageDetail$totalCount
    
    files <- append(files, count)
  }
  
  #Make data frame for just refuge file counts
  filesdf <- data.frame(codesdf$names, files)
  colnames(filesdf) <- c("name","fileCount")
  
  #Get terciles for color coding
  percentile <- ecdf(filesdf$fileCount)
  colors <- c()
  for (x in filesdf$fileCount){
    if(percentile(x) < 0.333){
      colors <- append(colors, "firebrick3")
    }else if(percentile(x) < 0.666){
      colors <- append(colors, "gold")
    }else{
      colors <- append(colors, "forestgreen")
    }
  }
  
  #Make the plot
  filesbyrefuge <- ggplot(filesdf,aes(x=name,y=fileCount,fill=name))+ 
    geom_bar(stat="identity", show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    labs(title = NULL, x = NULL, y = "References Added") +
    theme(
      text=element_text(family = "sans"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
      panel.background = element_rect(fill = "lightblue3"),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 15, face = "bold",color = "steelblue"),
      plot.margin = margin(1,1.5,1,1, "cm")
    )
  return(filesbyrefuge)
}
