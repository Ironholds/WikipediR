#Basic calling function
wiki_call <- function(URL, ...){
  
  #Make a GET request
  response <- GET(URL, ...)
  
  #Check the validity of the response from a server POV
  ConnectionErrorHandler(response)
  
  #Parse it
  parsed_response <- wiki_parse(response = response)
  
  #Check for API errors
  APIErrorHandler(parsed_response)
  
  #Return
  return(parsed_response)
}