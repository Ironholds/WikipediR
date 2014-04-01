#API connection constructor and testing class
ConnectionClass <- setRefClass(Class = "ConnectionClass",
                               fields = list(APIURL = "character",
                                             language = "character",
                                             project = "character",
                                             warnings = "character"),
                               methods = list(
                                 
                                 #Identify and generate the API URL
                                 URLConstructor = function(){
                                   
                                   #Load in the dataset of possible projects and languages
                                   data("wikis", package = "WikipediR", envir = environment())
                                   
                                   #Work out what combination is being asked for
                                   URLComponents <- wikis[wikis$code == .self$language & wikis$project == .self$project,]
                                   
                                   #Did it work?
                                   if(nrow(URLComponents) == 0){
                                     
                                     #If not, register the warning and return FALSE
                                     .self$warnings <- paste(.self$warnings,"The combination of language code and project provided is not recognised. See ?APIConnect and the 'wikis' RData file.")
                                     
                                     return(FALSE)
                                     
                                   }
                                    
                                   #Otherwise, construct the URL and add it to the object. Then return TRUE
                                   .self$APIURL <- paste(URLComponents$code,.self$project,"org",sep = ".")
                                   
                                   return(TRUE)
                                  
                                 },
                                 
                                 #Connection tester
                                 ConTester = function(){
                                   
                                   #Make a get request
                                   to_dispose <- GET(url = .self$APIURL)
                                     
                                   #If it succeeded return TRUE
                                   return(TRUE)
                                   
                                 },
                                 
                                 #Wrapper
                                 ConstructAndTest = function(){
                                   
                                   #Instantiate warning field
                                   .self$warnings <- ""
                                   
                                   #Generate the URL, and test that this worked
                                   if(!.self$URLConstructor()){
                                     
                                     return(FALSE)
                                     
                                   }
                                   
                                   #If it did work, test the connection
                                   if(!.self$ConTester()){
                                     
                                     return(FALSE)
                                     
                                   } else {
                                     
                                     #They both worked? Great. We're done here, then.
                                     return(TRUE)
                                   }
                                   
                                  }
                                 
                                )
                              )

#API connection function
APIConnect <- function(language, project = "wikipedia"){
  
  #Error checking
  #Is it a recognised project?
  if(!project %in% c("wikipedia","wikisource",
                     "wikiquote","wikiversity",
                     "wikivoyage","wikinews",
                     "wiktionary","wikibooks")){
    
    stop("The project provided is unrecognised or not supported. See ?APIConnect")
    
  }
  
  #Instantiate ConnectObject
  ConnectObject <- ConnectionClass$new(language = language,
                                       project = project)
  
  #Test
  testResults <- ConnectObject$ConstructAndTest()
  
  #Did it work?
  if(!testResults){
    
    #If it didn't work, return the warnings and errors that have been accumulating in the connection object
    stop(ConnectObject$warnings)
    
  } else {
    
    #Otherwise scrub and return
    ConnectObject$warnings <- NULL
    return(ConnectObject)
    
  }
  
}