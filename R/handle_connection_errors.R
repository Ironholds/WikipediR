#Handler for errors/warnings inside the connection itself.
handle_connection_errors <- function(unparsed_response){
  
  #If it isn't a 200 or 304 request...
  if(!unparsed_response$status_code %in% c("200","304")){
    
    #Stop and provide the response from the server
    stop("Request failure:", unparsed_response$statusmessage, call. = FALSE)
    
  }
  
  #Return invisibly if the above check isn't triggered
  return(invisible())
  
}