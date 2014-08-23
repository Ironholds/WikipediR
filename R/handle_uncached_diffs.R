#Handles uncached diffs
handle_uncached_diffs <- function(parsed_response){
  
  #Identify names
  names <- names(unlist(parsed_response))
  
  #Are there uncached pages?
  uncached <- sum(grepl(x = names, pattern = "diff.notcached"))
  if(uncached){
    
    #If so, warn
    warning("This request contained ",uncached," uncached diffs; these will not be returned", call. = FALSE)
    
  }

  #Return invisibly
  return(invisible())
}