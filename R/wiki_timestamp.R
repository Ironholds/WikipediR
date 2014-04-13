#Converts to- and from- MediaWiki timestamps
wiki_timestamp <- function(timestamp, to_wiki = FALSE){
  
  if(!to_wiki){
    
    #If the intent is to convert a MediaWiki timestamp to POSIX, do so
    to_return <- strptime(x = timestamp, format = "%Y%m%d%H%M%S")
    
  } else {
    
    #If it's the other way around, convert a POSIX timestampt to something MediaWiki friendly.
    to_return <- as.numeric(gsub(as.character(timestamp), pattern = "(-| |:)", replacement = ""))
    
  }
  
  #Return
  return(to_return)
}