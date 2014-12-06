#'@title Connector-generating function for WikipediR
#'
#'@description
#'
#'wiki_con generates a connector object necessary to use the other API calls in WikipediR
#'
#'@param language The two or three-letter project language code - for example,
#'en.wikiquote.org has the language code "en"
#'
#'@param project The project type - currently accepted types are wikipedia, species, commons,
#'wikisource, wikiquote, wikinews, wikibooks, wikiversity and wikivoyage.
#'
#'@param w_timeout The number of seconds before requests time out. Set to 5 by default.
#'
#'@param ua custom modifications to the user agent. To obey API etiquette, you should be
#'including contact information.
#'
#'@details
#'
#'wiki_con is designed to generate a connector object that can be passed into the
#'API-wrapping functions to retrieve data. At the moment, it consists almost entirely of a
#'pre-constructed URL; the intent is to have it allow for both OAuth and direct authentication
#'given time, to take advantage of the higher API limits given by being authenticated.
#'
#'The language code and project names provided by the user are checked against an object
#'containing all 'legitimate' permutations of languages and project types - in other words,
#'all Wikipedia wikis that are not private, closed, dead, or otherwise restricted.
#'This list is stored as an .RData file, "wikis", which will be updated as the package is updated.
#'
#'To allow for the likelihood that the active projects change in quantity and variety over time,
#'a function, \code{\link{RDataRebuild}}, is provided in this package to allow the end user to update the wiki
#'listings whenever they want.
#'
#'@importFrom httr user_agent
#'@seealso \code{\link{RDataRebuild}}
#'@export

#Constructor for the connection object
wiki_con <- function(language, project = c("wikipedia","commons","species",
                                           "wikisource","wikiquote","wikinews",
                                           "wikibooks","wikiversity","wikivoyage"),
                     w_timeout = 5,
                     ua = NULL){
  
  #Match arguments
  project <- match.arg(project)
  
  #Commons and Wikispecies have different URL formats, so those have to be handled in a hinky way.
  if(project %in% c("commons","species")){
    
    con_url <- paste0("http://", project, ".wikimedia.org/w/api.php?format=json")
    
  } else {
    
    #Load in the dataset of possible projects and languages
    data("wikis", package = "WikipediR", envir = environment())
    wikis <- wikis #Yes, this is stupid, but CRAN demands it. Bloody promise-based systems.
    
    #Work out what combination is being asked for
    URLComponents <- wikis[wikis$code == language & wikis$project == project,]
    
    #Did it work?
    if(nrow(URLComponents) == 0){
      
      #If not, stop
      stop("The language and/or project names are not recognised. Please see the WikipediR documentation, particularly ?wiki_con")
      
    } else {
      
      #Otherwise, construct a url
      con_url <- paste0("http://",paste(URLComponents$code,URLComponents$project,"org/w/api.php?format=json", sep = "."))
      
    }
  }
  
  #Whatever the result, construct a list serving as the connector object
  con_object <- list(URL = con_url,
                     Config = c(user_agent(paste("WikipediR - https://github.com/Ironholds/WikipediR", ua)),
                                timeout.ms = (w_timeout * 1000)))
                                       
  #Return
  return(con_object)
  
}