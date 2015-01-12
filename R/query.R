#General functions and error handlers for
#generic queries and query construction.

#'@importFrom httr GET
query <- function(url, ...){
  
  #Encode url, add "http://", query
  url <- paste0("http://",URLencode(url))
  response <- GET(url, ...)
  
  #Check the validity of the response
  stop_for_status(response)
  
  #Parse the response, check for API errors, return
  parsed_response <- response_parse(response = response)
  handle_API_errors(parsed_response)
  return(parsed_response)
}

#Constructor for the URL
url_gen <- function(language, project, domain = NULL){
  
  if(is.null(domain)){
    #Commons and Wikispecies have different URL formats, so those have to be handled in a hinky way.
    if(project %in% c("commons","species")){
      url <- paste0(project, ".wikimedia.org/w/api.php?format=json")
    } else {
      url <- paste0(language, "." ,project, ".org/w/api.php?format=json")
    }
  } else {
    url <- paste0(domain,"/w/api.php?format=json")
  }

  #Return
  return(url)
}

#'@importFrom httr content
#'@importFrom jsonlite fromJSON
response_parse <- function(response){
  
  #Convert it into a character vector
  response_text <- content(x = response, as = "text")
  
  #From there, turn it into an R object from JSON
  parsed_text <- fromJSON(txt = response_text, simplifyVector = FALSE)
  
  #Return
  return(parsed_text)
  
}