#Checks for missing users.
MissingUsersHandler <- function(parsed_response){
  
  #Check for missing values
  names_to_check <- names(unlist(parsed_response))
  
  missing_names <- sum(grepl(x = names_to_check, pattern = "users\\.missing"))
  if(length(missing_names)){
    
    warning(missing_names," of the provided usernames did not exist", call. = FALSE)
    
  }
  
  return(invisible())
  
  
}