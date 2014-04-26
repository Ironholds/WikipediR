#Handler for missing pages - pages requested that the API couldn't confirm existed.
MissingPagesHandler <- function(parsed_response){
  
  page_names <- names(unlist(parsed_response))
  
  #Are there returns from missing pages?
  missing_pages <- sum(grepl(x = page_names, pattern = "missing"))
  if(missing_pages){
    
    #If so, warn
    warning("This request contained ",missing_pages," invalid page titles", call. = FALSE)
    
  }
  
  #Otherwise, return invisibly
  return(invisible())
  
}