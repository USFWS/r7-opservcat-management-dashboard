get_arlisM <- function(inputRef){
  library(httr)
  library(jsonlite)
  
  #Filter by date
  filterDate <- list(list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "AfterDate",
    startDate = "2018-09-01"
  ))
  
  #Set parameters
  params <- list(
    people = query_ARLIScreators(),
    dates = filterDate
  )
  
  #API call
  json_output <- api_call(params)
  count <- json_output$pageDetail$totalCount
  count <- formatC(count, big.mark=",", digits = 0, format = "f")

  return(count)
}

#get_arlisM("Togiak")
