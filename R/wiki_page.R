#Retrieves the most recent revision for each provided page title.
wiki_page <- function(con, page, properties = c("text","revid")) {
  
  #Format and standardise pages and properties
  page <- LimitHandler(page, 1)
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Construct query to retrieve the parsed DOM of the page
  page_url <- paste(con$URL, "&contentformat=application/json&action=parse&page=", page, "&prop=", properties, sep = "")

  #Run  
  page_content <- wiki_call(URL = page_url, con$CurlOpts)
  
  #Return
  return(page_content)
}