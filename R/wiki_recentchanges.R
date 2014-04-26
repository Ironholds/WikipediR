wiki_recentchanges <- function(con, properties = c("user","userid","comment",
                                                   "parsedcomment","flags","timestamp",
                                                   "title","ids","sizes","redirect",
                                                   "loginfo","tags","sha1"),
                               type = c("edit","external","new","log"),
                               tag = NULL, dir = "newer", limit = 50, top = FALSE) {
  
  #Format and standardise type and properties
  type <- match.arg(arg = type, several.ok = TRUE)
  type <- paste(type, collapse = "|")
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  
  #Construct URL
  rc_url <- paste0(con$URL,"&action=query&list=recentchanges&rcdir=",dir,"&rcprop=",properties,"&rctype=",type,"&rclimit=",limit)
  
  #If tags have been provided, collapse them and append to rc_url
  if(!is.null(tag)){
    
    tag <- paste(tag, collapse = "|")
    rc_url <- paste0(rc_url,"&rctag=",tag)
    
  }
  
  #If "top" is desired, append that
  if(top){
    
    rc_url <- paste0(rc_url,"&rctoponly")
    
  }
  
  #Query
  rc_content <- wiki_call(URL = rc_url)
  
  #Return
  return(rc_content)
}