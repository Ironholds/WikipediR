context("Metadata")

test_that("backlinks can be retrieved through page_backlinks", {
  expect_true({page_backlinks("en","wikipedia", page = "Aaron Halfaker");TRUE})
})

test_that("backlinks from a specific namespace can be retrieved through page_backlinks", {
  expect_true({page_backlinks("en","wikipedia", page = "Aaron Halfaker", namespaces = 0);TRUE})
})