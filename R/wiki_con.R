#Constructor for the connection object
wiki_con <- function(language, project = c("wikipedia","wikisource","wikiquote","wikinews","wikibooks","wikiversity","wikivoyage")){
  
  #Match arguments
  project <- match.arg(project)
  
  #Load in the dataset of possible projects and languages
  data("wikis", package = "WikipediR", envir = environment())
  
  #Work out what combination is being asked for
  URLComponents <- wikis[wikis$code == language & wikis$project == project,]
  
  #Did it work?
  if(nrow(URLComponents) == 0){
    
    #If not, stop
    stop("The language and/or project names are not recognised. Please see the WikipediR documentation, particularly ?wiki_con")
    
  } else {
    
    #Otherwise, create an object of wiki_conClass and return it
    return(wiki_conClass$new(URL = paste0("http://",paste(URLComponents$code,URLComponents$project,"org/w/api.php?format=json", sep = "."))))
    
  }
  
}