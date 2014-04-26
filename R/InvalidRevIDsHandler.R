#Checks for invalid revision IDs, warns if they're found
InvalidRevIDsHandler <- function(parsed_response){
  
  #Retrieve the names
  rev_names <- names(unlist(parsed_response))
  
  #Are there invalid revIDs?
  bad_revs <- sum(grepl(x = rev_names, pattern = "badrevids"))
  if(bad_revs){
    
    #If so, warn
    warning("This request contained ",bad_revs," invalid revisionIDs", call. = FALSE)
    
  }
  
  #Otherwise, return invisibly
  return(invisible())
}