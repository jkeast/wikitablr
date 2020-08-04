library(testthat)
library(dplyr)
library(purrr)

# sites to use for testing
urls <- c(
  colleges = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts",
  presidents = "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age",
  linebreaks = "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting",
#  marvel = "https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films",
  ascii = "https://en.wikipedia.org/wiki/ASCII"
)

# makes list of tables
tables <- urls %>%
  map(read_wikitables) %>%
  flatten()

test_that("clean_wiki_names works", {

  expect_is(tables, "list")
  expect_length(tables, 13)
  expect_true(all(unlist(map(tables, class)) == "data.frame"))

  tables_clean <- clean_wiki_names(tables)

  # test clean_wiki_names()
  # test that column names clean as expected
  vars <- c("school", "location", "control", "type", "enrollment", "founded", "accreditation")
  expect_equal(names(tables_clean[[1]]), vars)

  # test that correctly passes arguments to janitor::clean_names()
  expect_equal(
    names(clean_wiki_names(tables[1], "all_caps") %>% pluck(1)),
    toupper(vars)
  )

  # test that footnotes are removed
  expect_true(all(!stringr::str_detect(names(tables_clean[[1]]), "\\[")))

  # test that removes columns without name -- note: need to do
  # remove footnotes before this works
  ## NOTE-- remove footnotes not working for this
  expect_lt(ncol(tables_clean[[12]]), ncol(tables[[12]]))

  # test that removes special characters in names
  ## clean_wiki_names won't work with dummy data-- not sure why
  dummy_data_1 <- list(tibble::tribble(
    ~`first@`, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  ))

  expect_false(
    dummy_data_1 %>%
      clean_wiki_names() %>%
      purrr::pluck(1) %>%
      names() %>%
      stringr::str_detect("\\@") %>%
      any()
  )

  # test clean_rows()
  # test that duplicate of header is removed (this one has a double header)
  expect_lt(
    tables %>%
      clean_rows() %>%
      pluck(4) %>%
      nrow(),
    tables %>%
      pluck(4) %>%
      nrow()
  )

  # test that rows with same values in all column are removed
  ## read_wikitables not working for marvel page
#  marvel1 <- marvel %>%
#    purrr::pluck(1)
#  expect_lt(nrow(clean_rows(marvel1)), nrow(marvel1))

  # test add_na()
  ## I'm confused about applying these functions to the dummy data because
  ## they are supposed to take in a list of data frames
  dummy_data <- list(tibble::tribble(
    ~first, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  ))

  # see if first column is converted to NA
  expect_true(
    dummy_data %>%
      add_na(to_na = "N/A") %>%
      pluck(1) %>%
      pull(first) %>%
      is.na() %>%
      all()
  )

  # test for special_to_na = FALSE-- doesn't work
  expect_true(
    add_na(dummy_data, special_to_na = FALSE) %>%
      pluck(1) %>%
      pull(1) %>%
      stringr::str_detect("\\?") %>%
      any(na.rm = TRUE)
  )

  # test convert_types()
  tables_clean %>%
    convert_types() %>%
    pluck(4) %>%
    pull(born) %>%
    expect_is("Date")

  # test remove_footnotes()
  # test that footnotes are removed
  ## ERROR: Error in colSums(!is.na(dat)) :
  # 'x' must be an array of at least two dimensions
  skip("bug in remove_footnotes")
  tables_clean %>%
    remove_footnotes() %>%
    pluck(1) %>%
    pull(founded) %>%
    stringr::str_detect("\\[") %>%
    any() %>%
    expect_false()
})


test_that("special_to_na works", {
  ascii_table <- read_wikitables("https://en.wikipedia.org/wiki/ASCII") %>%
    clean_wiki_names() %>%
    purrr::pluck(2)

  # test that special_to_na = FALSE returns the special character
  expect_false(
    ascii_table %>%
      dplyr::filter(dec == "33") %>%
      dplyr::pull(glyph) %>%
      is.na() %>%
      any()
  )
  expect_true(
    ascii_table %>%
      special_to_na_single() %>%
      dplyr::filter(dec == "33") %>%
      dplyr::pull(glyph) %>%
      is.na() %>%
      any()
  )
})


test_that("old cleaners work", {
  skip("Skipping old and possibly meaningless cleaner tests")
  ## I think all tests below this point should either be restructured and moved to test-cleaners
  ## or removed completely

  # remove_footnotes = TRUE -- not sure why this isn't working
  expect_false(presidents_clean[, 3] %>%
                 stringr::str_detect("\\[a\\]") %>%
                 any())

  # remove_footnotes = FALSE
  presidents_clean_footnotes <- read_wiki_table(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
                stringr::str_detect("[\\[a\\]]") %>%
                any())

  # to_na = "George Washington"
  presidents_clean_na <- read_wiki_table(presidents, to_na = "George Washington")
  # test to detect "George Washington" in column 2-- should be false
  expect_false(presidents_clean_na[, 2] %>%
                 stringr::str_detect("George Washington") %>%
                 any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean[, 5] %>%
                 stringr::str_detect("—") %>%
                 any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_3 <- read_wiki_table(colleges, table_number = 3, special_to_na = FALSE)
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
                stringr::str_detect("—") %>%
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
