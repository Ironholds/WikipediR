#'@title Timestamp conversion function
#'
#'@description \code{\link{wiki_timestamp}} converts MediaWiki timestamps to POSIX-compliant
#'timestamps, and vice-versa.
#'
#'@param timestamp The timestamp(s) you wish to convert, provided as a vector. If the intent
#'is to convert them into MediaWiki timestamps, they must be POSIX-compliant.
#'
#'@param to_wiki Whether the intent is to convert them from POSIX to MediaWiki. Set to
#'FALSE by default
#'
#'@export
wiki_timestamp <- function(timestamp, to_wiki = FALSE){
  
  if(!to_wiki){
    
    #If the intent is to convert a MediaWiki timestamp to POSIX, do so
    to_return <- strptime(x = as.character(timestamp), format = "%Y%m%d%H%M%S")
    
  } else {
    
    #If it's the other way around, convert a POSIX timestampt to something MediaWiki friendly.
    to_return <- as.numeric(gsub(as.character(timestamp), pattern = "(-| |:)", replacement = ""))
    
  }
  
  #Return
  return(to_return)
}