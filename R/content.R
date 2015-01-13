#Checks for invalid revision IDs, warns if they're found.
invalid_revs <- function(parsed_response){
  if(!is.null(parsed_response$query$badrevids)){
    warning("This request contained ",length(parsed_response$query$badrevids)," invalid revisionID(s)", call. = FALSE)
  }
  return(invisible())
}

#'@title Retrieves MediaWiki page content
#'
#'@description
#'wiki_page retrieves the DOM of a particular MediaWiki page,
#'as a HTML blob inside a JSON object.
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
#'@param page The title of the page you want to retrieve
#'
#'@param as_wikitext whether to retrieve the wikimarkup (TRUE) or the HTML (FALSE).
#'Set to FALSE by default.
#'
#'@param properties Properties associated with the page, namely "text" (the actual content)
#'and "revid" (the revision ID of the current version).
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{revision_diff}} for retrieving 'diffs' between revisions,
#'\code{\link{revision_content}} for retrieving the text of specified revisions.
#'
#'@export
page_content <- function(language = NULL, project = NULL, domain = NULL,
                         page, as_wikitext = FALSE, ...){
  
  #Format and construct URL.
  if(as_wikitext){
    properties <- c("wikitext","revid")
  } else {
    properties <- c("text","revid")
  }
  properties <- paste(properties, collapse = "|")
  page <- handle_limits(page, 1)  
  url <- url_gen(language, project, domain, "&action=parse&page=", page, "&prop=", properties)
  
  #Run  
  content <- query(url, ...)
  
  #Return
  return(content)
}

#'@title Retrieves MediaWiki revisions
#'
#'@description
#'Retrieves the content of a provided list of revisions from whichever MediaWiki instance you're
#'querying. Returns as wikimarkup.
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
#'@param revisions The revision IDs of each desired revision.
#'
#'@param properties Properties you're trying to retrieve about that revision, should you want to;
#'options include "ids" (the revision ID of the revision...which is pointless),
#'"flags" (whether the revision was 'minor' or not), "timestamp" (the timestamp of the revision),
#'"user" (the username of the person who made that revision), "userid"
#'(the userID of the person who made the revision),
#'"size" (the size, in uncompressed bytes, of the revision), "sha1" (the SHA-1 hash of
#'the revision text), "contentmodel" (the content model of the page, usually "wikitext"),
#'"comment" (the revision summary associated with the revision), "parsedcomment" (the same,
#'but parsed, generating HTML from any wikitext in that comment), "tags" (any tags associated
#'with the revision) and "flagged" (the revision's status under Flagged Revisions).
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso
#'\code{\link{revision_diff}} for diffs between revisions,
#'and \code{\link{page_content}} for the content a specific page currently has.
#'
#'@export

#Retrieves revision text
revision_content <- function(language = NULL, project = NULL, domain = NULL,
                             revisions, properties = c("content","ids","flags","timestamp",
                                                       "user","userid","size",
                                                       "sha1","contentmodel","comment",
                                                       "parsedcomment","tags"),
                             ...){
  
  #Format, construct URL.
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  revisions <- handle_limits(revisions, 50)
  url <- url_gen(language, project, domain,
                 "&rvcontentformat=text/x-wiki&action=query&prop=revisions&rvprop=",
                 properties, "&revids=",revisions)
  
  #Run
  content <- query(url, ...)
  
  #Check for invalid RevIDs
  invalid_revs(content)
  
  #Return
  return(content)
}

#'@title Generates a "diff" between a pair of revisions
#'
#'@description
#'wiki_diff generates a diff between two revisions in a MediaWiki page.
#'This is provided as an XML-parsable blob inside the returned JSON object.
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
#'@param revisions The revision IDs of each "start" revision.
#'
#'@param properties Properties you're trying to retrieve about that revision, should you want to;
#'options include "ids" (the revision ID of the revision...which is pointless),
#'"flags" (whether the revision was 'minor' or not), "timestamp" (the timestamp of the revision,
#'which can be parsed with \code{\link{wiki_timestamp}}),"user" (the username of the person
#'who made that revision), "userid" (the userID of the person who made the revision),
#'"size" (the size, in uncompressed bytes, of the revision), "sha1" (the SHA-1 hash of
#'the revision text), "contentmodel" (the content model of the page, usually "wikitext"),
#'"comment" (the revision summary associated with the revision), "parsedcomment" (the same,
#'but parsed, generating HTML from any wikitext in that comment), "tags" (any tags associated
#'with the revision) and "flagged" (the revision's status under Flagged Revisions).
#'
#'@param direction The direction you want the diff to go in from the revisionID you have provided.
#'Options are "prev" (compare to the previous revision on that page), "next" (compare to the next
#'revision on that page) and "cur" (compare to the current, extant version of the page).
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@section Warnings:
#'
#'MediaWiki's API is deliberately designed to restrict users' ability to make computing-intense requests
#'- such as diff computation. As a result, the API only allows requests for one uncached diff in
#'each request. If you ask for multiple diffs, some uncached and some cached, you will be provided
#' with the cached diffs, one of the uncached diffs, and a warning.
#' 
#'If you're going to be asking for a lot of diffs, some of which may not be cached, it may be more
#'sensible to retrieve the revisions themselves using \code{\link{revision_content}} and compute the
#'diffs yourself.
#'
#'@export
revision_diff <- function(language = NULL, project = NULL, domain = NULL,
                          revisions, properties = c("ids","flags","timestamp","user","userid","size",
                                                    "sha1","contentmodel","comment","parsedcomment",
                                                    "tags","flagged"),
                          direction = c("prev","next","cur"), ...){
  
  #Check and construct URL
  direction <- match.arg(direction)
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  revisions <- handle_limits(revisions, 50)
  url <- url_gen(language, project, domain, "&action=query&prop=revisions&rvprop=",
                 properties, "&rvdiffto=", direction, "&rvcontentformat=text/css&revids=",
                 revisions)
  
  #Retrieve the content, check for invalid RevIDs and uncached diffs,
  #return.
  content <- query(url, ...)
  invalid_revs(content)
  if(sum(grepl(x = names(unlist(content)), pattern = "diff.notcached"))){
    warning("This request contained uncached diffs; these will not be returned", call. = FALSE)
  }
  return(content)
}