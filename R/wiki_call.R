#Basic calling function
wiki_call <- function(URL){
  
  #Make a GET request
  response <- GET(URL)
  
  #Check the validity of the response
  wiki_checker(response)
  
  #Parse it and return
  return(wiki_parse(response = response))
}