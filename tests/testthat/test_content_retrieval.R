context("Content retrieval")

test_that("Wikitext content can be retrieved through page_content", {
  expect_true({page_content("en","wikipedia", page = "Barack Obama", as_wikitext=TRUE);TRUE})
})

test_that("HTML content can be retrieved through page_content", {
  expect_true({page_content("en","wikipedia", page = "Barack Obama", as_wikitext=FALSE);TRUE})
})

test_that("HTML content can be retrieved through revision_content", {
  expect_true({revision_content("en","wikipedia", revisions = "102342934");TRUE})
})

test_that("Diffs can be retrieved through revision_content", {
  expect_true({revision_diff("en","wikipedia", revisions = c("129122231","129122233"));TRUE})
})