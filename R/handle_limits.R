#Handles limitations on the number of unique values you can pass in.
handle_limits <- function(parameters, limit){
  
  #If there are more parameters than the limit...
  if(length(parameters) > limit){
    
    #Warn
    warning("This option accepts ", limit, " values; you have provided ", length(parameters),
            ". Only the first ", limit, " will be returned.", call. = FALSE)
    
    #Truncate and collapse
    parameters <- paste(parameters[1:limit], collapse = "|")
    
    #Return
    return(parameters)
    
  } else {
    
    #Otherwise, simply collapse and return
    parameters <- paste(parameters, collapse = "|")
    return(parameters)
    
  }
  
}