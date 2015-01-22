context("Metadata")

test_that("backlinks can be retrieved through page_backlinks", {
  expect_true({page_backlinks("en","wikipedia", page = "Aaron Halfaker");TRUE})
})

test_that("backlinks from a specific namespace can be retrieved through page_backlinks", {
  expect_true({page_backlinks("en","wikipedia", page = "Aaron Halfaker", namespaces = 0);TRUE})
})

test_that("links can be retrieved through page_links", {
  expect_true({page_links("en","wikipedia", page = "Aaron Halfaker");TRUE})
})

test_that("links from a specific namespace can be retrieved through page_links", {
  expect_true({page_links("en","wikipedia", page = "Aaron Halfaker", namespaces = 0);TRUE})
})

test_that("external links can be retrieved through page_links", {
  expect_true({page_external_links("en","wikipedia", page = "Aaron Halfaker");TRUE})
})

test_that("external links with a particular protocol can be retrieved through page_links", {
  expect_true({page_external_links("en","wikipedia", page = "Aaron Halfaker", protocol = "http");TRUE})
})

test_that("page info can be retrieved through page_info", {
  expect_true({page_info("en","wikipedia", page = "Aaron Halfaker");TRUE})
})

test_that("page info with specified params can be retrieved through page_info", {
  expect_true({page_info("en","wikipedia", page = "Aaron Halfaker", properties = "talkid");TRUE})
})
