#'@title Basic calling function
#'
#'@description
#'Function used as a component of other package functions that handles the actual
#'connection to the MediaWiki API
#'
#'@param URL the URL to pass to GET. Set by the calling function.
#'
#'@param ... any other parameters to pass through to GET.
#'
#'@importFrom httr GET
wiki_call <- function(URL, ...){
  
  #Encode URL. I'm amazed none of the ancestor packages do this.
  URL <- URLencode(URL)
  
  #Make a GET request
  response <- GET(URL, ...)
  
  #Check the validity of the response from a server POV
  handle_connection_errors(response)
  
  #Parse it
  parsed_response <- wiki_parse(response = response)
  
  #Check for API errors
  handle_API_errors(parsed_response)
  
  #Return
  return(parsed_response)
}