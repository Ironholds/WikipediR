context("Missing user handling")

test_that("Missing user handling recognises invalid usernames", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&list=users&ususers=X-Wing @Aliciousness")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_users(example_response), gives_warning("of the provided"))
  
})

test_that("Missing user handling recognises the number of invalid usernames", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&list=users&ususers=X-Wing @Aliciousness")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_users(example_response), gives_warning("1 of the provided"))
  
})

test_that("Missing user handling does not recognise valid usernames", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&list=users&ususers=Ironholds")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_missing_users(example_response), equals(NULL))
  
})