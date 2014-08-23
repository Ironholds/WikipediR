#Checks for invalid revision IDs, warns if they're found
handle_invalid_revIDs <- function(parsed_response){
  
  #Are there invalid revIDs?
  if(!is.null(parsed_response$query$badrevids)){
    
    #If so, warn
    warning("This request contained ",length(parsed_response$query$badrevids)," invalid revisionID(s)", call. = FALSE)
    
  }
  
  #Otherwise, return invisibly
  return(invisible())
}