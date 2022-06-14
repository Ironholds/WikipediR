#Missing user handler for user_information/user_contributions
missing_users <- function(parsed_response){
  
  #Check for missing values
  names_to_check <- names(unlist(parsed_response))
  missing_names <- sum(grepl(x = names_to_check, pattern = "users\\.missing"))
  if(missing_names){
    warning(missing_names," of the provided usernames did not exist", call. = FALSE)
  }
  return(invisible())
}

#'@title Retrieve user contributions
#'
#'@description Retrieves metadata associated with the most recent contributions by a
#'specified user.
#'
#'@param language The language code of the project you wish to query,
#'if appropriate.
#'
#'@param project The project you wish to query ("wikiquote"), if appropriate.
#'Should be provided in conjunction with \code{language}.
#'
#'@param domain as an alternative to a \code{language} and \code{project} combination,
#'you can also provide a domain ("rationalwiki.org") to the URL constructor, allowing
#'for the querying of non-Wikimedia MediaWiki instances.
#'
#'@param username The username of the user whose contributions you want to retrieve.
#'Due to limitations at the API end, you can only retrieve edits for one user at a time.
#'
#'@param properties The metadata you want associated with each edit. Potential metadata includes "ids"
#'(the revision ID of the revision, which can be passed into \code{\link{revision_content}}),
#'"title" (the name of the page that was edited), "timestamp", "comment" (the edit summary associated
#'with the revision), "parsedcomment" (the same, but parsed, generating HTML from any wikitext
#'in that comment), "size" (the size, in uncompressed bytes, of the edit), "sizediff" (the size
#'delta between this edit, and the last edit to the page), "flags" (whether the revision was 
#''minor' or not), and "tags" (any tags associated with the revision).
#'
#'@param mainspace A boolean flag; FALSE retrieves all of the most recent contributions, while
#'TRUE limits the retrieved contributions to those in the 'mainspace' - in other words, edits to
#'actual articles. Set to FALSE by default
#'
#'@param limit The number of edits to be retrieved. 50 is the maximum for logged-out API users,
#'and putting in more than 50 will generate a warning.
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param continue When more results are available, use this to continue as input parameter for next request.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{user_information}} for information about a specific user (or group of users),
#'and \code{recent_changes} for non-user-specific recent actions.
#'
#'@examples
#'
#'#Retrieve the timestamps of a user's recent contributions to the English-language Wikipedia
#'contribs <- user_contributions("en", "wikipedia", username = "Ironholds",
#'                               properties = "timestamp")
#'
#'#Retrieve the timestamps of a user's recent contributions to a non-Wikimedia wiki.
#'rw_contribs <- user_contributions(domain = "rationalwiki.org", username = "David Gerard",
#'                                  properties = "ids", limit = 1)
#' 
#' #Retrieve data with continue parameter
#' batch_1 <- user_contributions(domain = "rationalwiki.org", username = "David Gerard", 
#' properties = "ids", limit = 1)
#' batch_2 <- user_contributions(domain = "rationalwiki.org", username = "David Gerard", 
#' properties = "ids", limit = 1, continue = batch_1[["continue"]][["uccontinue"]])
#'                            
#'@export
user_contributions <- function(language = NULL, project = NULL, domain = NULL,
                               username, properties = c("ids", "title", "timestamp",
                                                        "comment", "parsedcomment", "size", 
                                                        "sizediff", "flags", "tags"),
                               mainspace = FALSE, limit = 50, clean_response = FALSE,
                               continue = NULL,
                               ...){
  
  #Perform checks, construct URL
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  username <- handle_limits(username, 1)
  url <- url_gen(language, project, domain)
  
  query_param <- list(
    action  = "query",
    list    = "usercontribs",
    uclimit = limit,
    ucuser  = username,
    ucprop  = properties
  )
  
  if (!is.null(continue)) {
    query_param <- append(query_param, list(uccontinue = continue))
  }
  
  #If only article contributions are desired, note that.
  if(mainspace){
    query_param$ucnamespace <- 0
  }
  
  #Get, check and return
  contribs_content <- query(url, "ucontribs", clean_response, query_param = query_param, ...)
  missing_users(contribs_content)
  return(contribs_content)
}

#'@title Retrieve user information
#'
#'@description
#'Retrieves information about a user, or set of users, from the MediaWiki API,
#'including registration date, gender and editcount.
#'
#'@param language The language code of the project you wish to query,
#'if appropriate.
#'
#'@param project The project you wish to query ("wikiquote"), if appropriate.
#'Should be provided in conjunction with \code{language}.
#'
#'@param domain as an alternative to a \code{language} and \code{project} combination,
#'you can also provide a domain ("rationalwiki.org") to the URL constructor, allowing
#'for the querying of non-Wikimedia MediaWiki instances.
#'
#'@param user_names The username(s) of the users you want information on - this should be provided
#'as a vector. There is a hard limit of 50 distinct users per query, set by MediaWiki's API;
#'in the event that you go over this, a warning will be issued and the query will only be
#'performed for the first 50 names in the vector.
#'
#'@param properties The user properties you're interested in. Applicable properties are
#'"blockinfo" (details about the user's block, if they are currently blocked), "groups"
#'(the user groups the user is a member of), "implicitgroups" (groups they are a member of
#'through inheritance, as a result of membership in other groups), "rights" (what permissions
#'their group membership grants them), "editcount" (how many non-deleted edits they have),
#'"registration" (the date when they registered), "emailable" (whether they are contactable
#'through Special:EmailUser) and "gender" (their provided gender).
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@section Warnings:
#'There are a few caveats with the data provided by \code{user_information}, mostly stemming from
#'historical inconsistencies and peculiarities in MediaWiki.
#'
#'\code{groups} and \code{implicitgroups} gives you the user's permissions and group membership
#'on the project you are querying, not their membership on all projects - while you can find out
#'if "Ironholds" is not a sysop on, say, enwiki, that doesn't mean they aren't a sysop elsewhere
#'- there is no universal, API-accessible user groups listing.
#'
#'As an extension of the lack of centrality in Wikimedia's infrastructure, \code{registration}
#'tells you the date their account was created on the wiki you are querying. If they initially
#'registered on that wiki, this is accurate - if they registered on a different wiki,
#'this instead reflects the date and time that they first visited the wiki you're querying
#'while logged-in. For users registered before 2006, when registration logging was introduced,
#'the \code{registration} value represents not when they first registered, but when their first
#'edit was, since that was used as an estimator for existing accounts when the field was first
#'populated.
#'
#'@seealso \code{\link{user_contributions}} for retrieving recent contributions made by
#'a particular user.
#'
#'@examples
#'#Retrieving information from a Wikimedia project
#'user_info <- user_information("en", "wikipedia", user_names = "David Gerard",
#'                              properties = "registration")
#'
#'#Non-Wikimedia projects
#'user_info <- user_information(domain = "rationalwiki.org", user_names = "David Gerard",
#'                              properties = "registration")
#'@export
user_information <- function(language = NULL, project = NULL, domain = NULL,
                             user_names, properties = c("blockinfo","groups","implicitgroups",
                                                       "rights","editcount","registration",
                                                       "emailable","gender"),
                             clean_response = FALSE, ...){
  
  #Check, construct URL
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  user_names <- handle_limits(user_names, 50)
  url <- url_gen(language, project, domain)
  query_param = list(
    action  = "query",
    list    = "users",
    usprop  = properties,
    ususers = user_names
    
  )

  #Retrieve the content, check it, return.
  user_content <- query(url, "uinfo", clean_response, query_param = query_param, ...)
  missing_users(user_content)
  return(user_content)
  
}