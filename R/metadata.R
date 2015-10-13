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
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
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
                           clean_response = FALSE, ...){
  
  url <- url_gen(language, project, domain, "&action=query&list=backlinks&bltitle=", page,
                 "&bldir=", direction)
  if(!is.null(namespaces)){
    url <- paste0(url,"&blnamespace=",paste(namespaces, collapse = "|"))
  }
  content <- query(url, "blink", clean_response, ...)
  return(content)
}

#'@title Retrieve a page's links
#'
#'@description
#'page_links, when provided with a page title, retrieves internal wikilinks from the
#'current revision of that page.
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
#'@param page the title of the page you want the links of.
#'
#'@param limit the number of links to retrieve. 50 by default; a maximum of 500 is set server-side.
#'
#'@param direction the direction to order the links in, by destination page ID: "ascending"
#'or "descending". Set to "ascending" by default.
#'
#'@param namespaces The namespaces to filter to. By default, links to any namespace
#'are retrieved: alternately, a numeric vector of accepted namespaces (which are described
#'\href{https://www.mediawiki.org/wiki/Manual:Namespace#Built-in_namespaces}{here}) can be
#'provided, and only backlinks from pages within those namespaces will be returned.
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@examples
#'#Links
#'links <- page_links("en","wikipedia", page = "Aaron Halfaker")
#'
#'#Namespace-specific links
#'mainspace_links <- page_links("en","wikipedia", page = "Aaron Halfaker", namespaces = 0)
#'@export
page_links <- function(language = NULL, project = NULL, domain = NULL,
                       page, limit = 50, direction = "ascending", namespaces = NULL,
                       clean_response = FALSE, ...){
  
  url <- url_gen(language, project, domain, "&action=query&prop=links&titles=", page,
                 "&pldir=", direction, "&pllimit=", limit)
  
  if(!is.null(namespaces)){
    url <- paste0(url,"&plnamespace=",paste(namespaces, collapse = "|"))
  }
  content <- query(url, "plink", clean_response, ...)
  return(content)  
}

#'@title Retrieve a page's links
#'
#'@description
#'page_external_links, when provided with a page title, retrieves external wikilinks from the
#'current revision of that page.
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
#'@param page the title of the page you want the links of.
#'
#'@param protocol limit links to those with certain link protocols. Options are listed
#'in Special:ApiSandbox's
#'\href{https://en.wikipedia.org/wiki/Special:ApiSandbox#action=query&prop=extlinks}{elprotocol field}.
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@examples
#'#Links
#'external_links <- page_external_links("en","wikipedia", page = "Aaron Halfaker")
#'
#'#Protocol-specific links
#'external_http_links <- page_external_links("en","wikipedia",
#'                                           page = "Aaron Halfaker", protocol = "http")
#'@export
page_external_links <- function(language = NULL, project = NULL, domain = NULL,
                                page, protocol = NULL, clean_response = FALSE,
                                ...){
  
  url <- url_gen(language, project, domain, "&action=query&prop=extlinks&titles=", page)
  if(!is.null(protocol)){
    url <- paste0(url,"&elprotocol=", protocol)
  }
  content <- query(url, "elink", clean_response, ...)
  return(content)
}

#'@title Retrieve information about a particular page
#'
#'@description
#'page_info, when provided with a page title, retrieves metadata about that page.
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
#'@param page the title of the page you want the metadata of.
#'
#'@param properties the properties you'd like to retrieve. Some properties (the pageID, namespace,
#'title, language, length and most recent revision ID, for example) are retrieved by default,
#'whatever is passed to \code{properties}: properties that can be explicitly retrieved include
#'the page's protection level ("protection"), the ID of the associated talk page, if applicable
#'("talkid"), the full, canonical URL ("url"), and the displayed page title ("displaytitle").
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@examples
#'#Metadata
#'page_metadata <- page_info("en","wikipedia", page = "Aaron Halfaker")
#'
#'@export
page_info <- function(language = NULL, project = NULL, domain = NULL, 
                      page, properties = c("protection","talkid","url", "displaytitle"),
                      clean_response = FALSE, ...){
  
  properties <- match.arg(arg = properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  url <- url_gen(language, project, domain, "&action=query&prop=info&inprop=", properties, "&titles=", page)
  content <- query(url, "pageinfo", clean_response, ...)
  return(content)
}