context("connection errors")

test_that("connection error handling recognises invalid responses", {
  
  #Example set.
  example_response <- GET("http://httpstat.us/404")
  
  #Test resulting value
  expect_that(handle_connection_errors(example_response), throws_error("Request failure"))
  
})

test_that("connection error handling provides error messages when available", {
  
  #Example set.
  example_response <- GET("http://httpstat.us/404")
  example_response <- c(example_response,
                        statusmessage = "cheese is a kind of meat")
  example_response$statusmessage <- "cheese is a kind of meat"
  
  #Test resulting value
  expect_that(handle_connection_errors(example_response), throws_error("cheese is a kind of meat"))
  
})

test_that("connection error handling does not catch valid responses", {
  
  #Example set.
  example_response <- GET("http://httpstat.us/200")
  
  #Test resulting value
  expect_that(handle_connection_errors(example_response), equals(NULL))
  
})