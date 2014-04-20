wiki_catpages <- function(con, categories, properties = c("title","ids","sortkey","sortkeyprefix","type","timestamp"),
                          type = c("page","subcat","file")){
  
  #Parse the category titles
  categories <- gsub(x = categories, pattern = " ", replacement = "_")
  categories <- gsub(x = categories, pattern = "^", replacement = "Category:")
  
  #Match and standardise properties
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
    
  #Match and standardise types
  type <- match.arg(type, several.ok = TRUE)
  type <- paste(type, collapse = "|")
  
  #Construct URL
  catpage_url <- paste(con$URL,"&action=query&list=categorymembers&cmtitle=",categories,"&cmprop=",properties,"&cmtype=",type, sep = "")

  #Query
  catpage_response <- wiki_call(URL = catpage_url, con$CurlOpts)
  
  #Return
  return(catpage_response)
}