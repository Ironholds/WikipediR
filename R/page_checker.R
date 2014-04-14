page_checker(page_content){
  
  page_names <- names(unlist(page_content))
  
  #Are there returns from missing pages?
  missing_pages <- sum(grepl(x = page_names, pattern = "missing"))
  if(missing_pages){
    
    #If so, warn
    warning("This request contained ",missing_pages," invalid page titles", call. = FALSE)
    
  }
  
}