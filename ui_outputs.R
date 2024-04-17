plot_refuge_progress <- function(start, end){
  # start <- 2011
  # end <- 2023
  
  filtered_refuge <- filter_totals(start, end, refuge_totals_df)
  filtered_arlis <- filter_totals(start, end, add_empty(arlis_totals_df))
  names <- return_refuge_df()$names
  counts <- c()
  counts_arlis <- c()
  for(i in 1:length(names)){
    counts <- append(counts, sum(filtered_refuge[,i+1]))
    counts_arlis <- append(counts_arlis, sum(filtered_arlis[,i+1]))
  }
  
  total <- counts
  other <- counts-counts_arlis
  arlis <- counts_arlis
  
  df <- data.frame(names, total, arlis)
  
  refuge_plot <- ggplot(df,aes(x=names, group=1, text=paste0("All Additions: ",total," \nARLIS Additions: ", arlis, "\nPercent ARLIS: ", format(round(arlis*100/total, 2), nsmall = 2), "%"))) +
    geom_bar(aes(y = total, fill = "Total"), stat="identity", fill = "black") +
    labs(title = NULL, x = NULL, y = "Reference Count", fill=NULL) +
    theme(
      axis.text.x = element_text(angle = 45, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white", hjust = -0.5),
      panel.background = element_rect(fill = "#E0F0F5"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 12, face = "bold"),
      plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(0,1,1,1, "cm"),
      plot.background = element_blank(),
      legend.position="right"
    )
  refuge_plot
  arlis_adjusted <-append(0,df$arlis)
  #total_adjusted <- append(0,df$total)
  
  p <- ggplotly(refuge_plot, tooltip = c("text"), height = 600, name = "Other") %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)), showlegend = TRUE) %>%
    add_trace(y=~arlis_adjusted, name = 'Added By ARLIS', hoverinfo = "skip", marker = list(color=c("#0072B2"))) %>%
    add_trace(y=~c(rep(0,13)), name = 'Total in ServCat', hoverinfo = "skip", marker = list(color=c("black")))

  return(p)
}

plot_arlis_progress_1 <- function(start, end){
  # start <- 2015
  # end <- 2023
  # arlis_totals_df <- arlis_df
  
  filtered_arlis <- filter_totals(start, end, arlis_totals_df)
  names <- colnames(filtered_arlis)[c(2:14)]
  counts <- c()
  for(name in names){
    counts <- append(counts, sum(filtered_arlis[[name]]))
  }
  
  df <- data.frame(names, counts)
  df$names <- factor(df$names, levels = df$names)
  
  refuge_plot <- ggplot(df,aes(x=as.factor(names), y=counts, group=1, text=paste("Additions: ",counts))) +
    geom_bar(stat = "identity", fill="black") +
    labs(title = NULL, x = NULL, y = "Reference Count", fill=NULL) +
    theme(
      axis.text.x = element_text(angle = 45, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "#dceef4"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 12, face = "bold"),
      plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(0,1,1,1, "cm"),
      plot.background = element_blank()
    )
  refuge_plot
  
  p <- ggplotly(refuge_plot, tooltip = c("text"), height = 590) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)))
  #p
  return(p)
}

plot_arlis_progress_2 <- function(start, end){
  # start <- 2015
  # end <- 2023
  # arlis_totals_df <- arlis_df
  
  filtered_arlis <- filter_totals(start, end, arlis_totals_df)
  names <- colnames(filtered_arlis)[c(15:17)]
  counts <- c()
  for(name in names){
    counts <- append(counts, sum(filtered_arlis[[name]]))
  }
  
  df <- data.frame(names, counts)
  df$names <- factor(df$names, levels = df$names)
  
  refuge_plot <- ggplot(df,aes(x=as.factor(names), y=counts, group=1, text=paste("Additions: ",counts))) +
    geom_bar(stat = "identity", fill=c("black", "#0072B2", "#CC79A7")) +
    #paste("References Added by ARLIS From", start, "to", end)
    labs(title = NULL, x = NULL, y = "Reference Count", fill=NULL) +
    theme(
      axis.text.x = element_text(angle = 45, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "#dceef4"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 12, face = "bold", margin = margin(r = 20)),
      plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(0,1,1,1, "cm"),
      plot.background = element_blank()
    )
  refuge_plot
  
  p <- ggplotly(refuge_plot, tooltip = c("text"), height = 590) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)))
  #p
  return(p)
}

plot_region_progress <- function(start, end){
  # start <- 2011
  # end <- 2023
  
  filtered_region <- filter_totals(start, end, region_totals_df)
  names <- colnames(filtered_region)[2:ncol(filtered_region)]
  counts <- c()
  for(name in names){
    counts <- append(counts, sum(filtered_region[[name]]))
  }
  
  df <- data.frame(names, counts)
  df$names <- factor(df$names, levels = df$names)
  
  refuge_plot <- ggplot(df,aes(x=as.factor(names), y=counts, group=1, text=paste("Additions: ",counts,"\nRegion: ",word(names,-1)))) +
    geom_bar(stat = "identity", fill=c(rep("black", 6), "#0072B2", "black")) +
    labs(title=NULL, x = NULL, y = "Reference Count", fill=NULL) +
    geom_hline(yintercept = counts[7], col = "#0072B2", size = 1, linetype = "dashed") +
    theme(
      #text=element_text(family = "mono"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "#dceef4"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 12, face = "bold"),
      plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(0,1,1,1, "cm"),
      plot.background = element_blank()
    )
  refuge_plot
  
  p <- ggplotly(refuge_plot, tooltip = c("text"), height = 540) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)))
  return(p)
}

plot_region_cumul <- function(){
  counts <- c()
  prev <- 0
  for(i in 1:length(region_totals_df$`Region 7`)){
    counts <- append(counts, region_totals_df$`Region 7`[i] + prev)
    prev <- counts[length(counts)]
  }
  
  years <- 2011:as.integer(format(Sys.Date(), "%Y"))
  
  change <- c(0)
  for(i in 2:length(counts)){
    change <- append(change, counts[i] - counts[i-1])
  }
  
  df <- data.frame(years, counts)
  yearPlot <- ggplot(df,aes(x=years,y=counts,group=1,text=paste0("Year: ", years, "\nCumulative Total: ",counts," \nNew Additions: +",change))) + 
    #dodgerblue4, cee8f0
    geom_line(color = "black", size = 1) +
    geom_point(color = "black", size = 2) +
    scale_x_continuous(breaks = df$years, labels = df$years) +
    labs(title = NULL, x = NULL, y = "Total References in ServCat\n") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      panel.background = element_rect(fill = "#dceef4"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#0072B2", size = 1.2),
      axis.title.y = element_text(color = "white", size = 12, face = "bold"),
      plot.margin = margin(0,1,1,1, "cm"),
      plot.background = element_blank()
    )
  p <- ggplotly(yearPlot, tooltip = c("text"), height = 630) %>% config(displayModeBar = FALSE) %>% layout(paper_bgcolor = "rgba(0,0,0,0)", hoverlabel = list(font=list(size=17)))
  p
  return(p)
}

get_arlis_total <- function(start, end){
  filtered_arlis <- filter_totals(start, end, arlis_totals_df)
  return(sum(filtered_arlis$All))
}

get_arlis_avg <- function(start, end){
  avg <- get_arlis_total(start,end)/(end-start+1)
  return(round(avg, 1))
}

get_arlis_regtotal <- function(start, end){
  filtered_arlis <- filter_totals(start, end, arlis_totals_df)
  return(sum(filtered_arlis$`Region 7 Refuges Total`) + sum(filtered_arlis$`Other Region 7`))
}

make_download_table <- function(inputDf){
  newRow <- c("Total")
  for (i in 2:(ncol(inputDf))){
    newRow <- append(newRow, sum(inputDf[,i]))
  }
  df <- rbind(inputDf, newRow)
  df
  return(df)
}
