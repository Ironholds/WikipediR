pagecat_checker <- function(pagecat_content){
  
  #Are there missing pages?
  miss_count <- sum(grepl(x = names(unlist(pagecat_content)),
                          pattern = "missing"))
  
  if(miss_count > 0){
    
    warning("Warning: ",miss_count," of the page titles you provided were invalid")
    
  }
  
  return(invisible())
  
  
}