#Check the result of wiki_diff for problems. We can at least warn about them.
diff_checker <- function(diff_content){
  
  diff_names <- names(unlist(diff_content))
  
  #Are there invalid revIDs?
  bad_revs <- sum(grepl(x = diff_names, pattern = "badrevids"))
  if(bad_revs){
    
    #If so, warn
    warning("This request contained ",bad_revs," invalid revisionIDs", call. = FALSE)
    
  }
  
  #Are there uncached pages?
  uncached <- sum(grepl(x = diff_names, pattern = "diff.notcached"))
  if(uncached){
    
    #If so, warn
    warning("This request contained ",uncached, "uncached revisions; these will not be returned", call. = FALSE)
    
  }
  
  #Return invisibly.
  return(invisible())
  
}