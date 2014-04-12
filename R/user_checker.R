user_checker <- function(user_content){
  
  #Check for missing values
  names_to_check <- names(unlist(user_content))
  
  if(length(names_to_check[names_to_check == "query.users.missing"]) > 0){
    
    warning(length(names_to_check[names_to_check == "query.users.missing"]),"of the provided usernames did not exist")
    
  }
  
  return(invisible())
  
}