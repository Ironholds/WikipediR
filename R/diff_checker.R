#Check the result of wiki_diff for problems. We can at least warn about them.
diff_checker <- function(diff_content){
  
  #Are there invalid revIDs?
  if(!is.null(diff_content$query$badrevids)){
    
    #If so, hold, strip the names and warn
    bad_ids <- unlist(diff_content$query$badrevids)
    names(bad_ids) <- NULL
    warning("This request contained invalid revisionIDs:", bad_ids, call. = FALSE)
    
  }
  
  #Return invisibly.
  return(invisible())
  
}