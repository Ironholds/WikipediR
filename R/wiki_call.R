#Basic calling function
wiki_call <- function(URL){
  
  #Make a GET request
  response <- GET(URL)
  
  #Check the validity of the response from a server POV
  wiki_checker(response)
  
  #Parse it
  parsed_response <- wiki_parse(response = response)
  
  #Is there an error at the API part?
  if(!is.null(parsed_response$error)){
    
    stop("The API returned an error, code: ",parsed_response$error$code)
    
  }
  
  #Otherwise, return
  return(parsed_response)
}