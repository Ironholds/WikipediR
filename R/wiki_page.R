#Retrieves the most recent revision for each provided page title.
wiki_page <- function(con, pages, properties = c("content","ids","flags","timestamp",
                                                         "user","userid","size",
                                                         "sha1","contentmodel","comment",
                                                         "parsedcomment","tags")) {
  
  #Format and standardise pages and properties
  pages <- gsub(x = pages, pattern = " ", replacement = "_")
  pages <- paste(pages, collapse = "|")
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Construct URL
  page_url <- paste(con$URL,"&rvcontentformat=text%2Fcss&action=query&prop=revisions&rvdir=older&rvprop=",properties,"&titles=",pages, sep = "")

  #Run
  page_content <- wiki_call(URL = page_url)
  
  #Check for issues.
  page_checker(page_content)
  
  #Return
  return(page_content)
}