#site to use for testing
url <- "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts"

#test that output is a dataframe
expect_equal(class(read_wiki_table(url)), "data.frame")
#test that column names clean as expected
expect_equal(names(read_wiki_table(url)), c("school", "location", "control", "type", "enrollment_2013_2014", "founded", "accreditation"))
#test that structure of data is the same, regardless of whether I remove footnotes
expect_equal(str(read_wiki_table(url, remove_footnotes = TRUE)), str(read_wiki_table(url, remove_footnotes = FALSE)))
