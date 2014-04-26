#Handler for errors/warnings inside the API request
APIErrorHandler <- function(parsed_response){
  
  #Check. Is there an error at the API part?
  if(!is.null(parsed_response$error)){
    
    stop("The API returned an error: ",parsed_response$error$code,"-", parsed_response$error$info)
    
  }
  
  #Return invisibly
  return(invisible())
  
}