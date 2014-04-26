#Retrieves a 'diff' between two revisions to a page
wiki_diff <- function(con,
                      revisions,
                      properties = c("ids","flags","timestamp","user","userid","size",
                                             "sha1","contentmodel","comment","parsedcomment",
                                             "tags","flagged"),
                      direction = c("prev","next","cur")){
  
  #Match args
  direction <- match.arg(direction)
  properties <- match.arg(properties, several.ok = TRUE)
  
  #Save the desired properties and check revisions
  properties <- paste(properties, collapse = "|")
  revisions <- LimitHandler(revisions, 50)
    
  #Construct the URL
  diff_url <- paste(con$URL,"&action=query&prop=revisions&rvprop=",properties,"&rvdiffto=",direction,"&rvcontentformat=text%2Fcss&revids=",revisions, sep = "")
  
  #Retrieve the content
  diff_content <- wiki_call(URL = diff_url, con$CurlOpts)
  
  #Check for invalid RevIDs
  InvalidRevIDsHandler(diff_content)
  
  #Check for uncached diffs
  UncachedDiffsHandler(diff_content)
  
  #Return
  return(diff_content)
}