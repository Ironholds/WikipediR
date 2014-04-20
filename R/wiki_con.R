#Constructor for the connection object
wiki_con <- function(language, project = c("wikipedia","commons","species",
                                           "wikisource","wikiquote","wikinews",
                                           "wikibooks","wikiversity","wikivoyage"),
                     w_timeout = 5){
  
  #Match arguments
  project <- match.arg(project)
  
  #Commons and Wikispecies have different URL formats, so those have to be handled in a hinky way.
  if(project %in% c("commons","species")){
    
    #Create and return an appropriate wiki_conClass object
    return(wiki_conClass$new(URL = paste0("http://",project,".wikimedia.org/w/api.php?format=json")))
    
  } else {
    
    #Load in the dataset of possible projects and languages
    data("wikis", package = "WikipediR", envir = environment())
    wikis <- wikis
    
    #Work out what combination is being asked for
    URLComponents <- wikis[wikis$code == language & wikis$project == project,]
    
    #Did it work?
    if(nrow(URLComponents) == 0){
      
      #If not, stop
      stop("The language and/or project names are not recognised. Please see the WikipediR documentation, particularly ?wiki_con")
      
    } else {
      
      #Otherwise, create an object of wiki_conClass and return it
      return(wiki_conClass$new(URL = paste0("http://",paste(URLComponents$code,URLComponents$project,"org/w/api.php?format=json", sep = ".")),
                               CurlOpts = list("timeout.ms" = w_timeout * 1000)))
      
    }
  }
}