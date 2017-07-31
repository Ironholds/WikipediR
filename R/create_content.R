#' request token for api action as signed in user
#'
#' helper function to request a user action token
#' 
#' @param url a URL body
#' 
#' @return a token string
#'
#' @importFrom magrittr "%>%"
#'
get_action_token <- function(url) {
  
  # get login token
  response <- httr::modify_url(
    url, 
    query = list(
      action = "query",
      meta = "tokens",
      format = "json"
    )
  ) %>% httr::GET(
  ) %>% httr::stop_for_status(
  )
  
  # parse the response, check for API errors
  parsed_response <- response_parse(
    response = response, out_class = "actiontoken"
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

#' wikimedia api page creation
#'  
#' Create pages or category-pages on a wikimedia instance. 
#'  
#' @param url a URL body
#' @param p_title vector with page title strings of new pages
#' @param p_text vector with page content strings of new pages
#' @param category switch to decide, if the pages should be
#' created as category-pages 
#' 
#' @return TRUE
#'
#' @export
create_pages <- function(url, p_title, p_text, category = FALSE){

  if(length(p_title) != length(p_text)) {
    stop(
      "The length of the vectors p_title and p_text 
      is not equal."
    )
  }
    
  # get action token
  token <- NA
  token <- get_action_token(url)
  if(token %>% is.na){
    stop("Problems with the action token request.")
  }
  
  # create page(s) (with progress bar)
  pb <- utils::txtProgressBar(
    min = 0, max = length(p_title), style = 3
  )
  
  res <- c()
  for (i in 1:length(p_title)) {
    res[i] <- create_page(
      url, p_title[i], p_text[i], category, token
    )
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  all(res) %>%
    return()
}

#' wikimedia api page creation (single pages)
#'  
#' helper function to do the actual api requests for page 
#' and category-page creation  
#'  
#' @param url a URL body
#' @param p_title page title string of new page
#' @param p_text page content string of new page
#' @param category switch to decide, if the page should be
#' created as category-page
#' @param token action token to perform the request
#' 
#' @return TRUE
#'
create_page <- function(url, p_title, p_text, category, token){
  
  # create page
  response <- httr::modify_url(
    url, 
    query = list(
      action = "edit",
      title = ifelse(
        category, 
        paste0("Category:", p_title),
        p_title
      ),
      text = p_text,
      format = "json"
    )
  ) %>% httr::POST(
    body = list(token = token)
  ) %>% httr::stop_for_status(
  )
  
  # parse the response, check for API errors
  parsed_response <- response_parse(
    response = response, out_class = "createpage"
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
