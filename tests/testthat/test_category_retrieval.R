context("Category retrieval")

test_that("page categories can be retrieved through categories_in_page", {
  expect_true({categories_in_page("en","wikipedia", page = "Barack Obama");TRUE})
})

test_that("Hidden page categories can be retrieved through categories_in_page", {
  expect_true({categories_in_page("en","wikipedia", page = "Barack Obama", show_hidden=T);TRUE})
})

test_that("Category members can be retrieved through categories_in_page", {
  expect_true({pages_in_category("en","wikipedia", categories = "1920 births");TRUE})
})

test_that("Category members can be retrieved through categories_in_page", {
  expect_true({pages_in_category("en","wikipedia", categories = "1920s births", type = "subcat");TRUE})
})

test_that("page categories can be retrieved through categories_in_page  with non-ASCII query", {
  # \u30dd\u30b1\u30e2\u30f3 is "Pokemon" in Japanese letters
  expect_true({categories_in_page("en","wikipedia", page = "\u30dd\u30b1\u30e2\u30f3");TRUE})
})