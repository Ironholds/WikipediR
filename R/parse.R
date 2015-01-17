parse_response <- function(x){
  UseMethod("parse_response", x)
}

parse_response.rchanges <- function(x){
  x <- x$query$recentchanges
  return(x)
}
parse_response.rcontent <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  return(x)
}

parse_response.rdiff <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  return(x)
}

parse_response.uinfo <- function(x){
  x <- x$query$users
  return(x)
}

parse_response.ucontribs <- function(x){
  x <- x$query$usercontribs
  results <- unlist(x)
  results <- data.frame(matrix(results, nrow=length(x), byrow=T), stringsAsFactors=F)
  names(results) <- names(x[[1]])
  return(results)
}
parse_response.catpages <- function(x){
  x <- x$query$categorymembers
  results <- unlist(x)
  results <- data.frame(matrix(results, nrow=length(x), byrow=T), stringsAsFactors=F)
  names(results) <- names(x[[1]])
  return(results)
}
parse_response.pagecats <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x,function(x){
    cats <- unlist(x$categories)
    cats <- data.frame(matrix(cats, nrow=length(x$categories), byrow=T), stringsAsFactors=F)
    names(cats) <- names(x$categories[[1]])
    x$categories <- cats
    return(x)
  })
  return(results)
}
parse_response.blink <- function(x){
  x <- x$query$backlinks
  results <- lapply(x,unlist)
  return(results)
}
parse_response.plink <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$links <- lapply(x$links,unlist)
    return(x)
  })
  return(results)
}
parse_response.elink <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$extlinks <- unlist(x$extlinks)
    names(x$extlinks) <- NULL
    return(x)
  })
  return(results)
}
parse_response.pageinfo <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$restrictiontypes <- unlist(x$restrictiontypes)
    return(x)
  })
  return(results)
}