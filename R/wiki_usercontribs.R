#Retrieves contributions associated with a specified user
wiki_usercontribs <- function(con,
                              user,
                              properties = c("ids", "title", "timestamp",
                                             "comment", "parsedcomment", "size", 
                                             "sizediff", "flags", "tags"),
                              mainspace = FALSE,
                              limit = 50){
  
  #Check and collapse properties
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Check that >1 users wasn't submitted
  user <- LimitHandler(user, 1)
  
  #Construct URL
  contribs_url <- paste(con$URL, "&action=query&list=usercontribs&uclimit=", limit,
                        "&ucuser=", user, "&ucprop=", properties, sep = "")

  #If only article contributions are desired, note that.
  if(mainspace){
    
    contribs_url <- paste(contribs_url, "&ucnamespace=0", sep = "")
    
  }
  
  #Get
  contribs_content <- wiki_call(contribs_url, con$CurlOpts)
    
  #Check
  MissingUsersHandler(contribs_content)
  
  #Return
  return(contribs_content)
}