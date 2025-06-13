#Function: api_call(params)
#Takes list of parameters formatted as json and returns the total in the result
#of an Advanced Search API call with one page
api_call <- function(params){
  #Make call
  url <- "https://iris.fws.gov/APPS/ServCatServices/servcat/v4/rest/AdvancedSearch/Composite?top=1"
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  #response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  
  #Get total number of resulting references
  total <- json_output$pageDetail$totalCount
  
  return(total)
}

# make_refuge_totals_df <- function(){
#   currentYear <- as.integer(format(Sys.Date(), "%Y"))
#   years <- 2011:currentYear
#   refuges <- return_refuge_df()$names
#   df <- data.frame(matrix(ncol = 14, nrow = length(years)))
#   colnames(df) <- append("Year", refuges)
#   df$Year <- years
#   for (i in refuges){
#     totals <- c()
#     for (j in years){
#       params <- list(
#         units = query_refuge(i), 
#         dates = query_year(j)
#       )
#       totals <- append(totals, api_call(params))
#     }
#     df[[i]] <- totals
#   }
#   
#   return(df)
# }


#Function: api_call_long(params)
#Takes list of parameters formatted as json and returns the result of an
#Advanced Search API call with page length 5000
api_call_long <- function(params){
  #Make call
  url <- "https://iris.fws.gov/APPS/ServCatServices/servcat/v4/rest/AdvancedSearch/Composite?top=2000"
  body <- toJSON(params, auto_unbox = TRUE)
  response <- POST(url = url, body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  #response <- POST(url = url, config = authenticate(":",":","ntlm"), body = body, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  return(json_output)
}

#FIX DESCRIPTION
#Function: return_df(params)
#Return master data frame for Advanced Search results given params list
return_org_list <- function(params){
  json_output <- api_call_long(params)
  
  orgs <- json_output$items$units
  
  if(length(orgs)>0){
    df <- data.frame(I(orgs))
  }else{
    df <- data.frame(matrix(ncol = 1, nrow = 0))
  }
  colnames(df) <- c("Units")
  return(df)
}

#Function: count_org_list(params)
#Given a dataframe of organization lists for references, returns a list of the
#total count of references for each refuge
count_org_list <- function(inputDf){
  refuges <- return_refuge_df()
  if(nrow(inputDf) > 0){
    totals <- c()
    for (i in 1:length(refuges$names)){
      cccs <- refuges$codes[[i]]
      subset <- inputDf[sapply(inputDf$Units, function(x) any(x %in% cccs)), ]
      count <- length(subset)
      totals <- append(totals, count)
    }
  }else{
    totals <- rep(0, length(refuges$names))
  }
  return(totals)
}

#Function: make_refuge_df()
#Makes dataframe of ServCat totals for each year for each refuge
make_refuge_totals_df <- function(){
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  years <- 2011:currentYear
  refuges <- return_refuge_df()$names
  df <- data.frame(matrix(ncol = 14, nrow = length(years)))
  colnames(df) <- c("Year", refuges)
  df$Year <- years
  for (year in years){
    params <- list(
      units = query_orgs(unlist(return_refuge_df()$codes)),
      dates = query_year(year)
    )
    totals <- count_org_list(return_org_list(params))
    for(i in 1:length(totals)){
      df[which(df[,1]==year), i+1] <- totals[i]
    }
  }
  return(df)
}

make_arlis_totals_df <- function(){
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  years <- 2015:currentYear
  refuges <- return_refuge_df()$names
  df <- data.frame(matrix(ncol = 18, nrow = length(years)))
  colnames(df) <- c("Year", refuges, "Region 7 Refuges Total", "Other Region 7", "Other Regions", "All")
  df$Year <- years
  
  #get refuge data
  r7_refuge_totals <- c()
  for (year in years){
    params <- list(
      units = query_orgs(unlist(return_refuge_df()$codes)),
      dates = query_year(year),
      people = query_ARLIScreators()
    )
    org_df <- return_org_list(params)
    r7_refuge_totals <- append(r7_refuge_totals, nrow(org_df))
    each_refuge_totals <- count_org_list(org_df)
    for(i in 1:length(each_refuge_totals)){
      df[which(df[,1]==year), i+1] <- each_refuge_totals[i]
    }
  }
  
  #get non-refuge data
  
  df$`Region 7 Refuges Total` <- r7_refuge_totals
  
  #regional
  r7_totals <- c()
  for (year in years){
    params <- list(
      regions = query_region(7),
      dates = query_year(year),
      people = query_ARLIScreators()
    )
    r7_totals <- append(r7_totals, api_call(params))
  }
  df$`Other Region 7` <- r7_totals - r7_refuge_totals
  
  #total
  total_totals <- c()
  for (year in years){
    params <- list(
      dates = query_year(year),
      people = query_ARLIScreators()
    )
    total_totals <- append(total_totals, api_call(params))
  }
  df$All <- total_totals
  df$`Other Regions` <- total_totals - r7_totals
  
  #return
  return(df)
}

make_region_totals_df <- function(){
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  years <- 2011:currentYear
  regions <- paste("Region", 1:8)
  df <- data.frame(matrix(ncol = 9, nrow = length(years)))
  colnames(df) <- c("Year", regions)
  df$Year <- years
  
  for (num in 1:8){
    totals <- c()
    for (year in years){
      params <- list(
        regions = query_region(num),
        dates = query_year(year)
      )
      df[which(df[,1]==year), num+1] <- api_call(params)
    }
  }
  
  return(df)
}

#Function: filter_totals(start, end, inputDf)
#Makes dataframe for plotting based on user slider inputs
filter_totals <- function(start, end, inputDf){
  df <- inputDf
  subset <- df[which((df$Year >= start) & (df$Year <= end)), ]
  return(subset)
}

add_empty <- function(inputDf){
  df <- inputDf
  for (i in c(2014, 2013, 2012, 2011)){
    df <- rbind(c(i, rep(0, 17)), df)
  }
  return(df)
}



#TEST SECTION

# print(df[,1])
# which(df[,1] == 2012)
# print(df[1,2])
# params <- list(units = query_orgs(unlist(return_refuge_df()$codes)), dates = query_year(2011), people = query_ARLIScreators())
# api_call_long(params)
# df4 <- return_org_list(params)
# count <- count_org_list(df4)
# 
# df4[2,1]

#df3 <- make_refuge_totals_df()
#df4 <- filter_refuge_totals(2012, 2022, make_refuge_totals_df())
#subset <- df3[which((df3$Year >= 2012) & (df3$Year <= 2014)), ]
#df5 <- make_arlis_totals_df()
