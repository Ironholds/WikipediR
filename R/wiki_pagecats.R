wiki_pagecats <- function(con, pages, properties = c("sortkey","timestamp","hidden"), limit = 50,
                          show_hidden = FALSE){
  
  #Normalise and save the properties
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Check the number of pages in the request
  pages <- LimitHandler(pages, 50)
  
  if(show_hidden){
    
    show_hidden <- "hidden"
    
  } else {
    
    show_hidden <- "!hidden"
    
  }
  
  #Construct the URL
  pagecat_url <- paste(con$URL,"&action=query&prop=categories&clprop=",properties,"&clshow=",show_hidden,"&cllimit=",limit,"&titles=",pages, sep = "")
  
  #Retrieve the content
  pagecat_content <- wiki_call(URL = pagecat_url, con$CurlOpts)
  
  #Check for invalid pageIDs
  MissingPagesHandler(pagecat_content)
  
  #Return
  return(pagecat_content)
}