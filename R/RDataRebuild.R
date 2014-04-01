#Function for rebuilding wikis.RData, if and when it's needed
RDataRebuild <- function(){
  
  #URL-queryin' function
  NOCquery <- function(x){
    
    #Open connection
    con <- url(description = as.character(x), open = "rt", blocking = TRUE, encoding = "UTF-8")
    
    #Retrieve data from the connection, as a vector
    results <- as.vector(
      scan(file = con,
           what = "character",
           nmax = -1,
           sep = "\n",
           quiet = TRUE)
    )
    
    #Close connection
    close(con)
    
    #Return results to parent environment
    return(results)
  }
  
  #List of wikis
  allwikilist <- as.list(c("http://noc.wikimedia.org/conf/s1.dblist",
                           "http://noc.wikimedia.org/conf/s2.dblist",
                           "http://noc.wikimedia.org/conf/s3.dblist",
                           "http://noc.wikimedia.org/conf/s4.dblist",
                           "http://noc.wikimedia.org/conf/s5.dblist",
                           "http://noc.wikimedia.org/conf/s6.dblist",
                           "http://noc.wikimedia.org/conf/s7.dblist"))
  
  #List of unused/prohibited wikis
  prohibitedlists <- as.list(c("http://noc.wikimedia.org/conf/closed.dblist",
                            "http://noc.wikimedia.org/conf/deleted.dblist",
                            "http://noc.wikimedia.org/conf/special.dblist",
                            "http://noc.wikimedia.org/conf/private.dblist",
                            "http://noc.wikimedia.org/conf/wikimedia.dblist"))
  
  #Generate a list of prohibited wikis and a list of all wikis
  prohibitedwikis <-unlist(lapply(prohibitedlists, NOCquery))
  allwikis <- unlist(lapply(allwikilist, NOCquery))
  
  #Parse down into a list of live, "production" wikis.
  prodwikis <- allwikis[! allwikis %in% prohibitedwikis]
  
  #Split
  match <- regexpr(pattern = "wik(tionary|isource|ibooks|ivoyage|iversity|iquote|inews|i)$", text = prodwikis)
  
  #Retrieve language codes
  lang_codes <- unlist(lapply(regmatches(prodwikis,match,invert = TRUE),function(x){return(x[[1]])}))
  
  #Create a data frame of the results
  wikis <- data.frame("code" = lang_codes,"project" = regmatches(prodwikis,match))
  
  #Replace underscores
  wikis$code <- gsub(x = wikis$code, pattern = "_", replacement = "-")
  
  #Replace wikis
  wikis$project <- gsub(x = wikis$project, pattern = "wiki$", replacement = "wikipedia")
  
  #Overwrite existing .RData file.
  save(wikis, file = file.path(path.package("WikipediR"),"data","wikis.RData"))
}