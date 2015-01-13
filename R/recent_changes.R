#'@title Retrieves entries from the RecentChanges feed
#'
#'@description
#'wiki_recentchanges retrieves a stream of entries from Special:RecentChanges, with a variety of
#'associated metadata and filtering (of both entries *and* that metadata.
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
#'@param properties Properties you're trying to retrieve about each entry, Options include
#'"user" (the username of the person responsible for that entry), "userid" (the userID of said
#'person), "comment" (the edit summary associated with the entry), "parsedcomment" (the same,
#'but parsed, generating HTML from any wikitext in that comment), "flags" (whether the revision
#'was 'minor' or not), "timestamp", "title" (the name of the page the entry affected), "ids"
#'(the page id, along with the old and new revision IDs when applicable) "sizes" (the size,
#'in uncompressed bytes, of the entry, and, in the case of revisions, the size of the edit
#'it displaced), "tags" (any tags associated with the revision) and "loginfo" (applicable only
#'to log entries, and consisting of log ID numbers, log types and actions, and so on) and "sha1"
#'(the SHA-1 hash of the revision text).
#'
#'@param type The type of entry you want to retrieve; can be any permutation of "edit" (edits to existing pages),
#'"external" (external actions that impact on the project - primarily wikidata changes),
#'"new" (the creation of new pages) and "log" (log entries). By default, all of these entry types
#'are included.
#'
#'@param tag Only return items with particular "tags", such as "mobile edit". NULL by
#'default.
#'
#'@param dir Should it go from newest to oldest ("newer"), or oldest to newest ("older")?
#'By default, set to "newer".
#'
#'@param limit The number of entries you'd like to return. By default, set to 50, which is
#'also the maximum number per-request for logged-out users.
#'
#'@param top Should the request only return "top" entries - in other words, the most recent
#'entry on a page? Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@export
recent_changes <- function(language = NULL, project = NULL, domain = NULL,
                           properties = c("user","userid","comment",
                                          "parsedcomment","flags","timestamp",
                                          "title","ids","sizes","redirect",
                                          "loginfo","tags","sha1"),
                           type = c("edit","external","new","log"),
                           tag = NULL, dir = "newer", limit = 50, top = FALSE, ...) {
  
  #Format and standardise, construct URL
  type <- match.arg(arg = type, several.ok = TRUE)
  type <- paste(type, collapse = "|")
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  url <- url_gen(language, project, domain, "&action=query&list=recentchanges&rcdir=",
                 dir, "&rcprop=", properties, "&rctype=", type, "&rclimit=", limit)
  if(!is.null(tag)){
    url <- paste0(url, "&rctag=", paste(tag, collapse = "|"))
  }
  if(top){
    url <- paste0(url, "&rctoponly")
  }
  
  #Query
  content <- query(url, ...)
  
  #Return
  return(content)
}