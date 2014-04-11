#Function for validating the return from API calls.
wiki_checker <- function(response){
  
  #If the query completed, great.
  if(response$status < 400){
    
    return(invisible())
    
  } else {
    
    #Otherwise, stop and provide the response from the server
    stop("Request failure:", response$statusmessage, call. = FALSE)
    
  }
  
  
}