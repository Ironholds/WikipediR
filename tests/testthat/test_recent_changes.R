context("Recent changes")

test_that("Recentchanges feed entries can be retrieved through recent_changes", {
  expect_true({recent_changes("en","wikipedia", limit=1);TRUE})
})

test_that("recent_changes respects tags", {
  expect_true({recent_changes("en","wikipedia", limit=1, tag = "mobile edit");TRUE})
})

test_that("recent_changes respects types", {
  expect_true({recent_changes("en","wikipedia", limit=1, type = "new");TRUE})
})

test_that("recent_changes respects directional changes", {
  expect_true({recent_changes("en","wikipedia", limit=1, top = TRUE);TRUE})
})
