wiki_userinfo <- function(con, usernames, properties = c("blockinfo","groups","implicitgroups",
                                                        "rights","editcount","registration",
                                                        "emailable","gender")){
  
  
  #Match args, and save them
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")

  if(length(usernames) > 50){
    
    warning("You have provided",length(usernames),"usernames. The MediaWiki API allows for a maximum of 50. Only the first 50 will be used.")
    
    usernames <- paste(usernames[1:50], collapse = "|")
    
  } else {
    
    usernames <- paste(usernames, collapse = "|")
    
  }
  
  #Construct the URL
  user_url <- paste(con$URL,"&action=query&list=users&usprop=",properties,"&ususers=",usernames,sep = "")
  
  #Retrieve the content
  user_content <- wiki_call(URL = user_url)
  
  #Check
  user_checker(diff_content = diff_content)
  
  #Return
  return(user_content)
}