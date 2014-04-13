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
  
  #Save the desired properties and revisions
  properties <- paste(properties, collapse = "|")
  revisions <- paste(revisions, collapse = "|")
  
  #Construct the URL
  diff_url <- paste(con$URL,"&action=query&prop=revisions&rvprop=",properties,"&rvdiffto=",direction,"&rvcontentformat=text%2Fcss&revids=",revisions, sep = "")
  
  #Retrieve the content
  diff_content <- wiki_call(URL = diff_url)
  
  #Check
  diff_checker(diff_content = diff_content)
  
  #Return
  return(diff_content)
}