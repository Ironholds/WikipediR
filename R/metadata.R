#'@title Retrieve a page's backlinks
#'
#'@description
#'page_backlinks, when provided with a page title, retrieves backlinks to that
#'page. Output can be filtered to specific namespaces.
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
#'@param page the title of the page you want the backlinks of.
#'
#'@param direction the direction to order the backlinks in, by linking page ID: "ascending"
#'or "descending". Set to "ascending" by default.
#'
#'@param namespaces The namespaces to filter to. By default, backlinks from any namespace
#'are retrieved: alternately, a numeric vector of accepted namespaces (which are described
#'\href{https://www.mediawiki.org/wiki/Manual:Namespace#Built-in_namespaces}{here}) can be
#'provided, and only backlinks from pages within those namespaces will be returned.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@section Warnings: as with \code{\link{pages_in_category}}, if the page
#'you are linking to does not exist, an empty list will be returned, without
#'any indication of an error.
#'
#'@examples
#'#Backlink
#'all_bls <- page_backlinks("en","wikipedia", page = "Aaron Halfaker")
#'
#'#Namespace-specific backlinks
#'mainspace_bls <- page_backlinks("en","wikipedia", page = "Aaron Halfaker", namespaces = 0)
#'@export
page_backlinks <- function(language = NULL, project = NULL, domain = NULL,
                           page, direction = "ascending", namespaces = NULL,
                           ...){
  
  #Construct URL
  url <- url_gen(language, project, domain, "&action=query&list=backlinks&bltitle=", page,
                 "&bldir=", direction)
  if(!is.null(namespaces)){
    url <- paste0(url,"&blnamespace=",paste(namespaces, collapse = "|"))
  }
  content <- query(url, ...)
  return(content)
}