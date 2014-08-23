context("API errors")

test_that("API error handling recognises successful requests", {
  
  #Example set.
  example_url <- "http://en.wikipedia.org/w/api.php?action=query&list=categorymembers&format=json&cmtitle=Category%3Afruits&cmprop=title&cmtype=page&cmlimit=1"
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_API_errors(example_response), equals(NULL))
  
})

test_that("API error handling recognises errors", {
  
  #Example set.
  example_url <- "http://en.wikipedia.org/w/api.php?action=query&list=categorymembers&format=json&cmtitle=Category%3Afruits&cmprop=title&cmtype=page&cmlimit=1"
  example_response <- wiki_parse(GET(example_url))
  example_response$error <- list(code = "",
                                 info = "")
  #Test resulting value
  expect_that(handle_API_errors(example_response), throws_error("API returned an error"))
  
})

test_that("API error handling reports error codes correctly", {
  
  #Example set.
  example_url <- "http://en.wikipedia.org/w/api.php?action=query&list=categorymembers&format=json&cmtitle=Category%3Afruits&cmprop=title&cmtype=page&cmlimit=1"
  example_response <- wiki_parse(GET(example_url))
  example_response$error <- list(code = "turnip",
                                 info = "")
  #Test resulting value
  expect_that(handle_API_errors(example_response), throws_error("turnip"))
  
})

test_that("API error handling reports error info correctly", {
  
  #Example set.
  example_url <- "http://en.wikipedia.org/w/api.php?action=query&list=categorymembers&format=json&cmtitle=Category%3Afruits&cmprop=title&cmtype=page&cmlimit=1"
  example_response <- wiki_parse(GET(example_url))
  example_response$error <- list(code = "turnip",
                                 info = "turnip error")
  #Test resulting value
  expect_that(handle_API_errors(example_response), throws_error("turnip error"))
  
})