#General functions and error handlers for
#generic queries and query construction.

#'@title base query function
#'@description not designed to be used by anyone except
#'a third-party reuser package, such as WikidataR
#'@param url a URL body
#'@param out_class the class to set on the output object; used within
#'WikidataR to indicate what response-cleaning method should be applied.
#'
#'@param clean_response whether to clean the response, using the method assigned
#'by out_class, or not.
#'
#'@param ... further arguments to httr's GET.
#'@export
#'@importFrom httr GET user_agent stop_for_status
#'@importFrom utils URLencode
query <- function(url, out_class, clean_response = FALSE, ...){
  
  #Encode url, add "http://", query
  url <- paste0("http://",utils::URLencode(url))
  args <- list(...)
  if(length(args) > 0 && "config" %in% class(args[[1]]) && "useragent" %in% names(args[[1]])){
    response <- httr::GET(url, ...)
  } else {
    response <- httr::GET(url, httr::user_agent("WikipediR - https://github.com/Ironholds/WikipediR"), ...)
  }
  
  #Check the validity of the response
  httr::stop_for_status(response)
  
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
  response_text <- httr::content(x = response, as = "text")
  
  #From there, turn it into an R object from JSON
  parsed_text <- jsonlite::fromJSON(txt = response_text, simplifyVector = FALSE)
  class(parsed_text) <- out_class
  
  #Return
  return(parsed_text)
  
}