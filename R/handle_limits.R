#Handles limitations on the number of unique values you can pass in.
handle_limits <- function(parameters, limit){
  if(length(parameters) > limit){
    warning("This option accepts ", limit, " values; you have provided ", length(parameters),
            ". Only the first ", limit, " will be returned.", call. = FALSE)
    parameters <- paste(parameters[1:limit], collapse = "|")
  } else {
    parameters <- paste(parameters, collapse = "|")
  }
  return(parameters)
}