#'@title response parser
#'
#'@description
#'Parses responses from MediaWiki's API.
#'
#'@param response the server response.
#'
#'@importFrom httr content
#'@importFrom jsonlite fromJSON
wiki_parse <- function(response){
  
  #Convert it into a character vector
  response_text <- content(x = response, as = "text")
  
  #From there, turn it into an R object from JSON
  parsed_text <- fromJSON(txt = response_text, simplifyVector = FALSE)
  
  #Return
  return(parsed_text)
  
}