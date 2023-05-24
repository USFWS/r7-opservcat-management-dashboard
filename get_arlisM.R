get_arlisM <- function(inputRef){
  library(httr)
  library(jsonlite)
  
  #Filter by creator
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
  
  #Filter by date
  filterDate <- list(list(
    order = 0,
    logicOperator = "",
    fieldName = "DateCreated",
    filter = "AfterDate",
    startDate = "2018-09-01"
  ))
  
  #Define url and params for API request
  url <- "https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/AdvancedSearch"
  params <- list(
    people = filterPeople
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
  count <- formatC(count, big.mark=",", digits = 0, format = "f")

  return(count)
}

get_arlisM("Togiak")
