context("Missing page handling")

test_that("Missing page handling handling recognises invalid page titles", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=categories&titles=this is a cruel and terrible example. Or something.")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_pages(example_response), gives_warning())
  
})

test_that("Missing page handling handling recognises the number of invalid page titles", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=categories&titles=this is a cruel and terrible example. Or something.")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_pages(example_response), gives_warning("1 invalid page"))
  
})

test_that("Missing page handling handling recognises successful requests", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=categories&titles=R (programming language)")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_pages(example_response), equals(NULL))
  
})