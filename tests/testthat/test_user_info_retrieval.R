context("User info")

test_that("Response of requests of limit = 1 using continuation is the same as response with limit = 2", {
  expect_true({
    domain <- "wikidata.org"
    project <- "wikidata"
    user <- c("Removena")
    
    # Get all contributions ---------------------------------------------------
    contributions_batch_1 <- user_contributions(domain = domain, project = project, username = user, limit = 1)
    rvcontinue <- contributions_batch_1[["continue"]][["uccontinue"]]
    contributions_batch_2 <- user_contributions(domain = domain, project = project, username = user, limit = 1, continue = rvcontinue)
    
    batch_list <- append(
      contributions_batch_1[["query"]][["usercontribs"]], 
      contributions_batch_2[["query"]][["usercontribs"]]
    )
    
    # test list
    contributions_test <- user_contributions(domain = domain, project = project, username = user, limit = 2)
    
    # check if lists are identical
    identical(batch_list, contributions_test[["query"]][["usercontribs"]])
    
    ; TRUE
  })
})