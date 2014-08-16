#' @title a reference class for connector objects
#' @description
#' Class for connector objects. Currently (relatively) a placeholder; will need to be fleshed out
#' more once oAuth authentication and logins are live.
#' @field URL the base URL of the connector object. A length-one character vector.
#' @field CurlOpts options to pass through to RCurl; a list.
#' 
#' @importFrom methods setRefClass
wiki_conClass <- setRefClass("wiki_conClass",
                             fields = list("URL" = "character",
                                           "CurlOpts" = "list"))