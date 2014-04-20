#Checks recentchanges feed responses for problems
rc_checker <- function(rc_content){
  
  #If there are warnings, well, warn.
  if(!is.null(rc_content$warnings)){
    
    warning(unlist(rc_content$warnings, use.names = FALSE))
    
  }
  
  return(invisible())
  
}