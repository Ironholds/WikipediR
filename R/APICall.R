#Basic, consistent calling function
APICall <- function(URL){
  
  #Make the call
  CallResults <- GET(url = URL)
  
  #Convert from JSON to a list
  DeJSONedResults <- convert(CallResults, as = "parsed")
  
  #Return
  return(DeJSONedResults)
  
}