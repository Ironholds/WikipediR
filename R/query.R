#General functions and error handlers for
#generic queries and query construction.

#'@importFrom httr GET user_agent stop_for_status
query <- function(url, out_class, clean_response = FALSE, ...){
  
  #Encode url, add "http://", query
  url <- paste0("http://",URLencode(url))
  response <- GET(url, user_agent("WikipediR - http://cran.r-project.org/web/packages/WikipediR/index.html"), ...)
  
  #Check the validity of the response
  stop_for_status(response)
  
  #Parse the response, check for API errors, return
  parsed_response <- response_parse(response = response, out_class = out_class)
  if(!is.null(parsed_response$error)){
    stop("The API returned an error: ", parsed_response$error$code,
         " - ", parsed_response$error$info)
  }
  if(clean_response){
    parsed_response <- parse_response(parsed_response)
  }
  
  return(parsed_response)
}

#Constructor for the URL
url_gen <- function(language, project, domain = NULL, ...){
  
  if(is.null(domain)){
    #Commons and Wikispecies have different URL formats, so those have to be handled in a hinky way.
    if(project %in% c("commons","species")){
      url <- paste0(project, ".wikimedia.org/w/api.php?format=json", ...)
    } else {
      url <- paste0(language, "." ,project, ".org/w/api.php?format=json", ...)
    }
  } else {
    url <- paste0(domain,"/w/api.php?format=json", ...)
  }
  
  #Return
  return(url)
}

#'@importFrom httr content
#'@importFrom jsonlite fromJSON
response_parse <- function(response, out_class){
  
  #Convert it into a character vector
  response_text <- content(x = response, as = "text")
  
  #From there, turn it into an R object from JSON
  parsed_text <- fromJSON(txt = response_text, simplifyVector = FALSE)
  class(parsed_text) <- out_class
  
  #Return
  return(parsed_text)
  
}