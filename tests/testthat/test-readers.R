# webpages for tests
colleges <- "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts"

presidents <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age"

linebreaks <- "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting"

test_that("read_wiki_raw works", {
  # read_wiki_raw tests
  # Reads first presidents table with all default parameters
  presidents_raw <- read_wikitables(presidents) %>%
    purrr::pluck(1)

  # Jessica's test
  # test that output is a dataframe
  expect_is(presidents_raw, "data.frame")

  # test that linebreaks in cells are replaced with commas
  linebreaks_raw <- read_wikitables(linebreaks) %>%
    purrr::pluck(1)
  expect_true(linebreaks_raw[, 1] %>%
                stringr::str_detect(",") %>%
                any())

  # test that linebreaks in cells are replaced with "/" if replace_linebreak = "/ "
  presidents_2 <- presidents %>%
    read_wikitables(replace_linebreak = "/ ") %>%
    purrr::pluck(1)

  # test to detect "/" in column 4
  expect_true(presidents_2[, 4] %>%
                stringr::str_detect("/") %>%
                any())
  # test to detect "/" in column 5
  expect_true(presidents_2[, 5] %>%
                stringr::str_detect("/") %>%
                any())
})

test_that("read_wikitables works", {

  # read_wiki_table tests

  # reads and cleans presidents table 1 with all default parameters

  # reads and cleans colleges table 3 with all default parameters
  colleges_clean <- read_wiki_table(colleges, table_number = 3)

  # replace_linebreak = ", " (default)
  expect_true(read_wiki_table(linebreaks)[, 1] %>%
                str_detect(",") %>%
                any())

  # replace_linebreak = "/ "
  presidents_clean_2 <- read_wiki_table(presidents, replace_linebreak = "/ ")

  # test to detect "/" in column 4
  expect_true(presidents_clean_2[, 4] %>%
                str_detect("/") %>%
                any())
  # test to detect "/" in column 5
  expect_true(presidents_clean_2[, 5] %>%
                str_detect("/") %>%
                any())

  # remove_footnotes = TRUE -- not sure why this isn't working
  expect_false(presidents_clean[, 3] %>%
                 str_detect("\\[a\\]") %>%
                 any())

  # remove_footnotes = FALSE
  presidents_clean_footnotes <- read_wiki_table(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
                str_detect("[\\[a\\]]") %>%
                any())

  # to_na = "George Washington"
  presidents_clean_na <- read_wiki_table(presidents, to_na = "George Washington")
  # test to detect "George Washington" in column 2-- should be false
  expect_false(presidents_clean_na[, 2] %>%
                 str_detect("George Washington") %>%
                 any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean[, 5] %>%
                 str_detect("—") %>%
                 any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_3 <- read_wiki_table(colleges, table_number = 3, special_to_na = FALSE)
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
                str_detect("—") %>%
                any(na.rm = TRUE))

  # test that clean_rows() removes double header FAILS TEST
  presidents_raw <- read_wiki_raw(presidents)
  expect_lt(nrow(presidents_clean), nrow(presidents_raw))

  # Another test for the same thing
  presidents_noheader <- presidents_raw %>%
    clean_rows()
  expect_equal(nrow(presidents_noheader), nrow(presidents_clean))

  # Jessica's tests
  # test that structure of dataframes from both readers is the same
  # I suspect that this test should not always work based on the above two tests
  expect_identical(dim(presidents_clean), dim(presidents_raw))

  # test that structure of data is the same, regardless of whether I remove footnotes
  expect_equal(names(presidents_clean), names(presidents_clean_footnotes))
})

test_that("read_all_tables works", {

  # read_all_tables tests

  # Reads and cleans presidents tables with all default parameters
  presidents_clean <- read_all_tables(presidents)
  presidents_clean_1 <- presidents_clean[[1]]

  # Reads and cleans colleges tables with all default parameters
  colleges_clean <- read_all_tables(colleges)
  colleges_clean_3 <- colleges_clean[[3]]

  # Jessica's tests
  # test that structure is list
  expect_is(presidents_clean, "list")

  # test that length of list is equal to number of tables on page
  expect_equal(length(presidents_clean), 2)

  # replace_linebreak = ", " (default)
  expect_true(read_all_tables(linebreaks)[[1]][, 1] %>%
                str_detect(",") %>%
                any())

  # replace_linebreak = "/ "
  presidents_clean_2 <- read_all_tables(presidents, replace_linebreak = "/ ")
  pres_clean_2_1 <- presidents_clean_2[[1]]
  # test to detect "/" in column 4
  expect_true(pres_clean_2_1[, 4] %>%
                str_detect("/") %>%
                any())
  # test to detect "/" in column 5
  expect_true(pres_clean_2_1[, 5] %>%
                str_detect("/") %>%
                any())

  # remove_footnotes = TRUE -- not sure why this works for this one but not for read_wiki_table
  expect_false(presidents_clean_1[, 3] %>%
                 str_detect("[\\[a\\]]") %>%
                 any())

  # remove_footnotes = FALSE
  pres_clean_footnotes <- read_all_tables(presidents, remove_footnotes = FALSE)
  pres_clean_footnotes_1 <- pres_clean_footnotes[[1]]

  presidents_clean_footnotes <- read_wiki_table(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
                str_detect("[\\[a\\]]") %>%
                any())

  # to_na = "George Washington"
  presidents_clean_na <- read_all_tables(presidents, to_na = "George Washington")
  pres_clean_na_1 <- presidents_clean_na[[1]]
  # test to detect "George Washington" in column 2-- should be false
  expect_false(pres_clean_na_1[, 2] %>%
                 str_detect("George Washington") %>%
                 any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean_3[, 5] %>%
                 str_detect("—") %>%
                 any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_all <- read_all_tables(colleges, special_to_na = FALSE)
  colleges_3 <- colleges_all[[3]]
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
                str_detect("—") %>%
                any())

  # test that clean_rows() removes double header PASSES TEST
  presidents_raw <- read_wiki_raw(presidents)
  expect_lt(nrow(presidents_clean_1), nrow(presidents_raw))

  # Different test for the same thing
  presidents_noheader <- presidents_raw %>%
    clean_rows()
  expect_equal(nrow(presidents_noheader), nrow(presidents_clean_1))
})


test_that("natalias tests work", {

  # tests -- natalia

  # test that special_to_na = FALSE returns the special character
  expect_equal((read_wiki_table("https://en.wikipedia.org/wiki/ASCII", table_number = 2, special_to_na = FALSE, remove_footnotes = FALSE) %>% filter(dec == "33") %>% pull(glyph)), "!")


  # test that special_to_na = TRUE returns NA
  expect_equal((read_wiki_table("https://en.wikipedia.org/wiki/ASCII", table_number = 2, special_to_na = TRUE, remove_footnotes = TRUE) %>% filter(dec == "33") %>% pull(glyph) %>% is.na()), TRUE)


  #### THIS is where the error is
  expect_equal((read_wiki_table("https://en.wikipedia.org/wiki/ASCII", table_number = 2, special_to_na = FALSE, remove_footnotes = TRUE) %>% filter(dec == "33") %>% pull(glyph)), "!")


  # test that special_to_na = TRUE returns NA
  expect_equal((read_wiki_table("https://en.wikipedia.org/wiki/ASCII", table_number = 2, special_to_na = TRUE, remove_footnotes = FALSE) %>% filter(dec == "33") %>% pull(glyph) %>% is.na()), TRUE)


})
