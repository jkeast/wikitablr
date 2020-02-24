#sites to use for testing
college <- read_wiki_raw("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts")

presidents <- read_wiki_raw("https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age")

marvel <- read_wiki_raw("https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films")

#test clean_wiki_names()
#test that column names clean as expected
expect_equal(names(clean_wiki_names(college)), c("school", "location", "control", "type", "enrollment_2013_2014", "founded", "accreditation"))
#test that correctly passes arguments to janitor::clean_names()
expect_equal(names(clean_wiki_names(college, "all_caps")), c("SCHOOL", "LOCATION", "CONTROL", "TYPE", "ENROLLMENT_2013_2014", "FOUNDED", "ACCREDITATION"))
#test that footnotes are removed
expect_equal(all(!stringr::str_detect(names(clean_wiki_names(college)), "\\[")), TRUE)


#test clean_rows()
#test that duplicate of header is removed (this one has a double header)
expect_lt(nrow(clean_rows(presidents)), nrow(presidents))
#test that rows with same values in all column are removed
expect_lt(nrow(clean_rows(marvel)), nrow(marvel))


#test add_na()
dummy_data <- as.data.frame(rbind(c("?", "two", "three"),
                    c("_", "five", 7),
                    c("N/A", "ten", "eleven")))
#see if first column is converted to NA
expect_equal(all(is.na(add_na(dummy_data, to_na = "N/A")$V1)), TRUE)




#test convert_types()
expect_equal(class(convert_types(clean_rows(presidents))$Born), "Date")


#test remove_footnotes()
#test that footnotes are removed
expect_equal(all(!stringr::str_detect(remove_footnotes(college)$Founded, "\\[")), TRUE)


#test -- natalia
read_wiki_table("https://en.wikipedia.org/wiki/ASCII", table_number = 2, special_to_na = FALSE, remove_footnotes = FALSE)

