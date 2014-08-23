context("limit handling")

test_that("limit handling recognises input lengths and limits correctly", {
  
  #Example set.
  example_sequence <- seq(1,30,1)
  
  #Test resulting value
  expect_that(handle_limits(example_sequence, 10), gives_warning("30"))
  expect_that(handle_limits(example_sequence, 10), gives_warning("10"))
  
})

test_that("limit handling outputs sufficient values", {
  
  #Example set.
  example_sequence <- seq(1,10,1)
  
  #Test resulting value
  expect_that(length(gregexpr(text = handle_limits(example_sequence, 10), pattern = "\\|")[[1]]),
              equals(9))
})