test_that("readers work", {
  #site to use for testing
  url <- "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts"

  #test for read_wiki_raw()
  #test that output is a dataframe
  expect_equal(class(read_wiki_raw(url)), "data.frame")

  #tests for read_wiki_table
  #test that structure of dataframes from both readers is the same
  expect_identical(dim(read_wiki_table(url)), dim(read_wiki_raw(url)))
  #test that structure of data is the same, regardless of whether I remove footnotes
  expect_equal(str(read_wiki_table(url, remove_footnotes = TRUE)), str(read_wiki_table(url, remove_footnotes = FALSE)))


  #tests for read_all_tables()
  #test that structure is list
  expect_equal(class(read_all_tables(url)), "list")
  #test that length of list is equal to number of tables on page
  expect_equal(length(read_all_tables(url)), 3)

})
