wiki_pagecats <- function(con,
                            pages,
                            properties = c("sortkey","timestamp","hidden"),
                            limit = 50,
                            show_hidden = FALSE){
  
  #Normalise and save the various parameters
  pages <- gsub(x = pages, pattern = " ", replacement = "_")
  pages <- paste(pages, collapse = "|")
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  if(show_hidden){
    
    show_hidden <- "hidden"
    
  } else {
    
    show_hidden <- "!hidden"
    
  }
  
  #Construct the URL
  pagecat_url <- paste(con$URL,"&action=query&prop=categories&clprop=",properties,"&clshow=",show_hidden,"&cllimit=",limit,"&titles=",pages, sep = "")
  
  #Retrieve the content
  pagecat_content <- wiki_call(URL = pagecat_url)
  
  #Check
  pagecat_checker(pagecat_content = pagecat_content)
  
  #Return
  return(pagecat_content)
}