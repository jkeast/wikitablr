library(testthat)
library(dplyr)
library(purrr)

# webpages for tests
urls <- c(
  colleges = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts",
  presidents = "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age",
  linebreaks = "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting"
)

test_that("read_wikinodes works", {
  # read_wiki_raw tests
  # Reads first presidents table with all default parameters
  raw <- urls %>%
    map(read_wikinodes)

  # test that each item is a list
  expect_is(raw, "list")
  expect_length(raw, 3)
  expect_true(
    raw %>%
      map_chr(class) %>%
      `==`("xml_nodeset") %>%
      all()
  )
  expect_true(
    raw %>%
      map(~map_chr(., class)) %>%
      unlist() %>%
      `==`("xml_node") %>%
      all()
  )

  # test that linebreaks in cells are replaced with commas
  expect_true(
    raw %>%
      pluck("linebreaks") %>%
      pluck(1) %>%
      rvest::html_table(fill = TRUE) %>%
      pull(1) %>%
      stringr::str_detect(",") %>%
      any()
  )

})

test_that("read_wikitables works", {
  raw <- urls %>%
    map(read_wikitables)

  # test that each item is a list
  expect_is(raw, "list")
  expect_length(raw, 3)
  expect_true(
    raw %>%
      map_chr(class) %>%
      `==`("list") %>%
      all()
  )
  expect_true(
    raw %>%
      map(~map_chr(., class)) %>%
      unlist() %>%
      `==`("data.frame") %>%
      all()
  )

  # test that linebreaks in cells are replaced with "/" if replace_linebreak = "/ "
  presidents_table_one <- urls[["presidents"]] %>%
    read_wikitables(replace_linebreak = "/ ") %>%
    clean_wiki_names() %>%
    pluck(1)

  # test to detect "/" in column 4
  expect_true(
    presidents_table_one %>%
      pull(4) %>%
      stringr::str_detect(",") %>%
      any()
  )

  # test to detect "/" in column 5
  expect_true(
    presidents_table_one %>%
      pull(5) %>%
      stringr::str_detect(",") %>%
      any()
  )

  # replace_linebreak = ", " (default)
  expect_true(
    raw %>%
      pluck("linebreaks") %>%
      pluck(1) %>%
      pull(1) %>%
      stringr::str_detect(",") %>%
      any()
  )

  # replace_linebreak = "/ "
  linebreaks_table_2 <- urls[["linebreaks"]] %>%
    read_wikitables(replace_linebreak = "/ ") %>%
    clean_wiki_names() %>%
    pluck(2)

  # test to detect "/" in column 1
  # FAILS
  expect_false(
    linebreaks_table_2 %>%
      pull(1) %>%
      stringr::str_detect("/") %>%
      any()
  )

})


