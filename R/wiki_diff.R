#Retrieves a 'diff' between two revisions to a page
wiki_diff <- function(con,
                      revID,
                      rvprop = c("ids","flags","timestamp","user","userid","size",
                                             "sha1","contentmodel","comment","parsedcomment",
                                             "tags","flagged"),
                      direction = c("prev","next","cur")){
  
  
  #Match args
  direction <- match.arg(direction)
  rvprop <- match.arg(rvprop, several.ok = TRUE)
  
  #Save the desired properties
  rvprop <- paste(rvprop, collapse = "|")
  
  #Construct the URL
  diff_url <- paste(con$URL,"&action=query&prop=revisions&rvprop=",rvprop,"&rvdiffto=",direction,"&rvcontentformat=text%2Fcss&revids=",revID, sep = "")
  
  #Retrieve the content
  diff_content <- wiki_call(URL = diff_url)
  
  #Check
  diff_checker(diff_content = diff_content)
  
  #Return
  return(content)
}