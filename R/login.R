#' request token to start client login
#'
#' helper function to request a user login token 
#' 
#' @param url a URL body
#' @param user a username of a registered user
#' 
#' @return a token string
#'
#' @importFrom magrittr "%>%"
#'
get_prelogin_token <- function(url, user) {
  
  # get login token
  response <- httr::modify_url(
    url, 
    query = list(
      action = "login",
      lgname = user,
      format = "json"
    )
  ) %>% httr::POST(
  ) %>% httr::stop_for_status(
  )
  
  # parse the response, check for API errors
  parsed_response <- response_parse(
    response = response, out_class = "prelogintoken"
  )
  if(!is.null(parsed_response$error)){
    stop(
      "The API returned an error: ", 
      parsed_response$error$code,
      " - ", parsed_response$error$info
    )
  }
  
  parse_response(parsed_response) %>%
    return()
}

#' wikimedia api user login
#'
#' Login to a wikimedia instance to trigger api requests
#' as a registered user. This function only allows the 
#' very basic login with username and password. Wikimedia
#' setups that require more sophisticated login methods 
#' are not supported. 
#' 
#' @param url a URL body
#' @param user a username of a registered user
#' @param pw the password of said user
#' 
#' @return TRUE
#'
#' @export
login <- function(url, user, pw){
  
  # get prelogin token
  token <- get_prelogin_token(url, user)
  
  # login
  response <- httr::modify_url(
    url, 
    query = list(
      action = "clientlogin",
      username = user,
      rememberMe = 1,
      loginreturnurl = url,
      format = "json"
    )
  ) %>% 
    httr::POST(
      body = list(
        logintoken = token,
        password = pw
    )
  ) %>% httr::stop_for_status(
  )
  
  # parse the response, check for API errors
  parsed_response <- response_parse(
    response = response, out_class = "login"
  )
  if(!is.null(parsed_response$error)){
    stop(
      "The API returned an error: ", 
      parsed_response$error$code,
      " - ", parsed_response$error$info
    )
  }
  
  parse_response(parsed_response) %>%
    return()
}
