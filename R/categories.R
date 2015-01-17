#'@title Retrieves categories associated with a page.
#'
#'@description
#'Retrieves categories associated with a page (or list of pages) on a MediaWiki instance
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
#'@param pages A vector of page titles, with or without spaces, that you want to retrieve
#'categories for.
#'
#'@param properties The properties you want to retrieve about the categories.
#'Options are "sortkey" (the key that sorts the way the page is stored in each category),
#'"timestamp" (when the category was added to that page) and "hidden" (tags those categories
#'in the returned list that are 'hidden' from readers).
#'
#'@param limit The maximum number of categories you want to retrieve for each page. Set
#'to 50 by default.
#'
#'@param show_hidden Whether or not to include 'hidden' categories in the categories
#'that are retrieved - these are usually associated with the maintenance of Wikipedia
#'and its internal processes. Set to FALSE by default.
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{pages_in_category}} for pages in a specified category.
#'
#'@examples
#'
#'#Retrieve the categories for the "New Age" article on en.wiki
#'cats <- categories_in_page("en", "wikipedia", pages = "New Age")
#'
#'#Retrieve the categories for the "New Age" article on rationalwiki.
#'rw_cats <- categories_in_page(domain = "rationalwiki.org", pages = "New Age")
#'@export
categories_in_page <- function(language = NULL, project = NULL, domain = NULL,
                               pages, properties = c("sortkey","timestamp","hidden"),
                               limit = 50, show_hidden = FALSE, clean_response = FALSE,
                               ...){
  
  #Check, construct URL
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  pages <- handle_limits(pages, 50)
  if(show_hidden){
    show_hidden <- "hidden"
  } else {
    show_hidden <- "!hidden"
  }
  url <- url_gen(language, project, domain,
                 paste0("&action=query&prop=categories&clprop=", properties,
                        "&clshow=", show_hidden, "&cllimit=", limit,
                        "&titles=",pages))
  
  #Retrieve, check, return
  content <- query(url, "pagecats", clean_response, ...)
  page_names <- names(unlist(content))
  missing_pages <- sum(grepl(x = page_names, pattern = "missing"))
  if(missing_pages){
    warning("This request contained ",missing_pages," invalid page title(s)", call. = FALSE)
  }
  return(content)
}

#'@title 
#'Retrieves a list of category members.
#'
#'@description
#'wiki_catpages retrieves a list of pages, subcategories, files or all of the above
#'in a specified category (or series of specified categories)
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
#'@param categories The names of the categories you want to gather information for.
#'
#'@param properties The properties you want to gather for each member of the category. 
#'Options are "title" (the name of the member, including namespace), 
#'"id" (the unique numeric identifier of the member), "sortkey" 
#'(the hexadecimal key used to sort that member within the category), 
#'"sortkeyprefix" (the human-readable sort key), "type"
#'(whether the member is a page, a subcategory or a file) and 
#'"timestamp" (when the member was added to the category)
#'
#'@param type The type of member you're interested in returning;
#'options are any permutation of "page" (pages), "subcat" (subcategories) and "file" (files).
#'
#'@param clean_response whether to do some basic sanitising of the resulting data structure.
#'Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET().
#'
#'@section warnings:
#'Because of the way MediaWiki stores this data, both "the category you asked for doesn't exist"
#'and "the category you asked for exists, but has no members" return in the same way.
#'
#'@seealso \code{\link{categories_in_page}} for finding categories that a specified page is a member of.
#'
#'@examples
#'
#'#Retrieve the pages in the "New Age" category on en.wiki
#'cats <- pages_in_category("en", "wikipedia", categories = "New Age")
#'
#'#Retrieve the pages in the "New Age" category on rationalwiki.
#'rw_cats <- pages_in_category(domain = "rationalwiki.org", categories = "New Age")
#'@export
pages_in_category <- function(language = NULL, project = NULL, domain = NULL, categories,
                              properties = c("title","ids","sortkey","sortkeyprefix","type","timestamp"),
                              type = c("page","subcat","file"), clean_response = FALSE,
                              ...){
  
  #Format and check
  categories <- gsub(x = categories, pattern = "^", replacement = "Category:")
  categories <- handle_limits(categories, 50)
  properties <- match.arg(properties, several.ok = TRUE)
  properties <- paste(properties, collapse = "|")
  type <- match.arg(type, several.ok = TRUE)
  type <- paste(type, collapse = "|")
  
  #Construct URL
  url <- url_gen(language, project, domain, "&action=query&list=categorymembers&cmtitle=",
                categories, "&cmprop=", properties, "&cmtype=",type)
  
  #Query and return
  content <- query(url, "catpages", clean_response, ...)
  return(content)
}