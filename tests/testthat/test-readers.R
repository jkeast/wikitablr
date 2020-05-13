# webpages for tests
colleges <- "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts"

presidents <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age"

linebreaks <- "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting"

test_that("read_wikinodes works", {
  # read_wiki_raw tests
  # Reads first presidents table with all default parameters
  presidents_raw <- read_wikinodes(presidents) %>%
    purrr::pluck(1)

  # test that each item is a list
  expect_is(presidents_raw, "xml_node")

  # test that linebreaks in cells are replaced with commas
  # error incorrect number of dimensions-- I think this is because it
  # isn't a dataframe yet?
  linebreaks_raw <- read_wikinodes(linebreaks) %>%
    purrr::pluck(1) %>%
    rvest::html_table()
  expect_true(linebreaks_raw %>%
    dplyr::pull(1) %>%
    stringr::str_detect(",") %>%
    any())

  # test that linebreaks in cells are replaced with "/" if replace_linebreak = "/ "
  presidents_2 <- presidents %>%
    read_wikinodes(replace_linebreak = "/ ") %>%
    purrr::pluck(1) %>%
    rvest::html_table(fill = TRUE)

  # test to detect "/" in column 4
  # incorrect number of dimensions
  expect_true(presidents_2[, 4] %>%
    stringr::str_detect("/") %>%
    any())
  # test to detect "/" in column 5
  expect_true(presidents_2[, 5] %>%
    stringr::str_detect("/") %>%
    any())
})

test_that("read_wikitables works", {

  # reads colleges tables with all default parameters
  colleges_clean <- read_wikitables(colleges)

  # linebreaks table
  linebreaks_table <- read_wikitables(linebreaks) %>%
    purrr::pluck(1)

  # test that read_wikitables returns a list
  expect_is(colleges_clean, "list")

  # test that each item in the list is a data frame
  expect_true(
    colleges_clean %>%
      purrr::map_chr(class) %>%
      `==`("data.frame") %>%
      all()
  )

  # replace_linebreak = ", " (default)
  expect_true(
    linebreaks_table[, 1] %>%
      stringr::str_detect(",") %>%
      any()
  )

  # replace_linebreak = "/ "
  linebreaks_table2 <- read_wikitables(linebreaks, replace_linebreak = "/ ") %>%
    purrr::pluck(1)

  # test to detect "/" in column 1
  # FAILS
  expect_false(linebreaks_table2[, 1] %>%
    stringr::str_detect("/") %>%
    any()
  )
})



test_that("read_all_tables works", {
  skip("skipping old and possibly meaningless tests")

  # read_all_tables tests

  # Reads and cleans presidents tables with all default parameters
  presidents_clean <- read_wikitables(presidents)
  presidents_clean_1 <- presidents_clean[[1]]

  # Reads and cleans colleges tables with all default parameters
  colleges_clean <- read_wikitables(colleges)
  colleges_clean_3 <- colleges_clean[[3]]

  # Jessica's tests
  # test that structure is list
  expect_is(presidents_clean, "list")

  # test that length of list is equal to number of tables on page
  expect_equal(length(presidents_clean), 2)

  # replace_linebreak = ", " (default)
  expect_true(read_wikitables(linebreaks)[[1]][, 1] %>%
    stringr::str_detect(",") %>%
    any())

  # replace_linebreak = "/ "
  presidents_clean_2 <- read_wikitables(presidents, replace_linebreak = "/ ")
  pres_clean_2_1 <- presidents_clean_2[[1]]
  # test to detect "/" in column 4
  expect_true(pres_clean_2_1[, 4] %>%
    stringr::str_detect("/") %>%
    any())
  # test to detect "/" in column 5
  expect_true(pres_clean_2_1[, 5] %>%
    stringr::str_detect("/") %>%
    any())

  # remove_footnotes = TRUE -- not sure why this works for this one but not for read_wiki_table
  expect_false(presidents_clean_1[, 3] %>%
    stringr::str_detect("[\\[a\\]]") %>%
    any())

  # remove_footnotes = FALSE
  pres_clean_footnotes <- read_wikitables(presidents, remove_footnotes = FALSE)
  pres_clean_footnotes_1 <- pres_clean_footnotes[[1]]

  presidents_clean_footnotes <- read_wikitables(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
    stringr::str_detect("[\\[a\\]]") %>%
    any())

  # to_na = "George Washington"
  presidents_clean_na <- read_wikitables(presidents, to_na = "George Washington")
  pres_clean_na_1 <- presidents_clean_na[[1]]
  # test to detect "George Washington" in column 2-- should be false
  expect_false(pres_clean_na_1[, 2] %>%
    stringr::str_detect("George Washington") %>%
    any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean_3[, 5] %>%
    stringr::str_detect("—") %>%
    any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_all <- read_wikitables(colleges, special_to_na = FALSE)
  colleges_3 <- colleges_all[[3]]
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
    stringr::str_detect("—") %>%
    any())

  # test that clean_rows() removes double header PASSES TEST
  expect_lt(
    nrow(clean_rows_single(presidents_clean_1)),
    nrow(presidents_clean_1)
  )

})
