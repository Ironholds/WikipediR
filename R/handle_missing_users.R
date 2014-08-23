#Checks for missing users.
handle_missing_users <- function(parsed_response){
  
  #Check for missing values
  names_to_check <- names(unlist(parsed_response))
  
  missing_names <- sum(grepl(x = names_to_check, pattern = "users\\.missing"))
  if(missing_names){
    
    warning(missing_names," of the provided usernames did not exist", call. = FALSE)
    
  }
  
  return(invisible())
  
  
}