#Handles limitations on the number of unique values you can pass in.
#Takes an input set of params and a limit, and warns of the length of one
#is greather than the other, limiting the set if so, and either way,
#collapsing for incorporation into the API query.
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