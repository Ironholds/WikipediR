#Retrieves revision text
wiki_revision <- function(con, revisions, properties = c("content","ids","flags","timestamp",
                                                         "user","userid","size",
                                                         "sha1","contentmodel","comment",
                                                         "parsedcomment","tags")) {
  
  #Format and standardise properties
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Check provided revisions against the limit
  revisions <- LimitHandler(revisions, 50)
  
  #Construct URL
  revision_url <- paste(con$URL,"&rvcontentformat=text/x-wiki&action=query&prop=revisions&rvprop=",properties,"&revids=",revisions, sep = "")
  
  #Run
  revision_content <- wiki_call(URL = revision_url, con$CurlOpts)
  
  #Check for invalid RevIDs
  InvalidRevIDsHandler(revision_content)
  
  #Return
  return(revision_content)
}