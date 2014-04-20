#Class for connector objects. Currently (relatively) a placeholder; will need to be fleshed out more
#Once oAuth authentication and logins are live.
wiki_conClass <- setRefClass("wiki_conClass",
                             fields = list("URL" = "character",
                                           "CurlOpts" = "list"))