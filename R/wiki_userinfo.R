wiki_userinfo <- function(con, usernames, properties = c("blockinfo","groups","implicitgroups",
                                                        "rights","editcount","registration",
                                                        "emailable","gender")){
  
  #Match args, and save them
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Check that the number of usernames provided is below the limit.
  usernames <- LimitHandler(usernames, 50)
  
  #Construct the URL
  user_url <- paste(con$URL,"&action=query&list=users&usprop=",properties,"&ususers=",usernames,sep = "")
  
  #Retrieve the content
  user_content <- wiki_call(URL = user_url, con$CurlOpts)
  
  #Check for missing users
  MissingUsersHandler(user_content)
  
  #Return
  return(user_content)
  
}