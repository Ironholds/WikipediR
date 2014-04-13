#Basic calling function
wiki_call <- function(URL){
  
  #Make a GET request
  response <- GET(URL)
  
  #Check the validity of the response
  wiki_checker(response)
  
  #Parse it
  parsed_response <- wiki_parse(response = response)
  
  #Is there an error at the API end?
  if(!is.null(parsed_response$error)){
    
    stop("The API returned an error, code: ",parsed_text$error$code)
    
  }
  
  #Otherwise, return
  return(parsed_response)
}