context("uncached diff handling")

test_that("uncached diff handling handling does not recognise valid, cached diffs", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=revisions&&rvdiffto=prev&rvcontentformat=text/css&revids=1")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_uncached_diffs(example_response), equals(NULL))
  
})

test_that("uncached diff handling handling recognises invalid, uncached diffs", {
  
  #Example set.
  example_url <- URLencode("http://en.wikipedia.org/w/api.php?format=json&action=query&prop=revisions&&rvdiffto=cur&rvcontentformat=text/css&revids=1|2000|19000|19|17")
  example_response <- wiki_parse(GET(example_url))
  
  #Test resulting value
  expect_that(handle_uncached_diffs(example_response), gives_warning())
  
})