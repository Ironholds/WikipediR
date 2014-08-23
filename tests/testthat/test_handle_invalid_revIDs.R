context("invalid revIDs")

test_that("revID error handling recognises invalid revIDs", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&rvcontentformat=text/x-wiki&action=query&prop=revisions&&revids=-1|-2")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_invalid_revIDs(example_response), gives_warning())
  
})

test_that("revID error handling counts invalid revIDs correctly", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&rvcontentformat=text/x-wiki&action=query&prop=revisions&&revids=-1|-2")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_invalid_revIDs(example_response), gives_warning("2 invalid revisionID"))
  
})

test_that("revID error handling recognise mixed valid and invalid revIDS", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&rvcontentformat=text/x-wiki&action=query&prop=revisions&&revids=1|-2")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_invalid_revIDs(example_response), gives_warning("1 invalid revisionID"))
  
})

test_that("revID error handling does not incorrectly recognise valid revIDS", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&rvcontentformat=text/x-wiki&action=query&prop=revisions&&revids=1")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_invalid_revIDs(example_response), equals(NULL))
  
})