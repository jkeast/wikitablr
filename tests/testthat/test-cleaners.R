test_that("cleaners work", {
  # sites to use for testing
  college <- read_wikitables("https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts")

  presidents <- read_wikitables("https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age")

  marvel <- read_wikitables("https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films")

  ascii <- read_wikitables("https://en.wikipedia.org/wiki/ASCII")

  # test clean_wiki_names()
  # test that column names clean as expected
  vars <- c("school", "location", "control", "type", "enrollment", "founded", "accreditation")
  clean_college <- clean_wiki_names(college)
  clean_college1 <- clean_college %>%
    purrr::pluck(1)
  expect_equal(names(clean_college1), vars)

  # test that correctly passes arguments to janitor::clean_names()
  expect_equal(
    names(clean_wiki_names(college, "all_caps") %>% purrr::pluck(1)),
    toupper(vars)
  )

  # test that footnotes are removed
  expect_true(all(!stringr::str_detect(names(clean_college1), "\\[")))

  # test that removes columns without name -- note: need to do
  # remove footnotes before this works
  ## NOTE-- remove footnotes not working for this
  ascii_clean <- ascii %>%
    #remove_footnotes() %>%
    clean_wiki_names() %>%
    purrr::pluck(1)
  expect_lt(ncol(ascii_clean), ncol(ascii %>% purrr::pluck(1)))

  # test that removes special characters in names
  ## clean_wiki_names won't work with dummy data-- not sure why
  dummy_data_1 <- tibble::tribble(
    ~`first@`, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  )

  expect_false(
    dummy_data_1 %>%
      # still doesn't work. Why?
      clean_wiki_names() %>%
      names() %>%
      stringr::str_detect("\\@") %>%
      any()
  )

  # test clean_rows()
  # test that duplicate of header is removed (this one has a double header)
  expect_lt(
    presidents %>%
      clean_rows() %>%
      purrr::pluck(1) %>%
      nrow(),
    presidents %>%
      purrr::pluck(1) %>%
      nrow()
  )

  # test that rows with same values in all column are removed
  ## read_wikitables not working for marvel page
  marvel1 <- marvel %>%
    purrr::pluck(1)
  expect_lt(nrow(clean_rows(marvel1)), nrow(marvel1))

  # test add_na()
  ## I'm confused about applying these functions to the dummy data because
  ## they are supposed to take in a list of data frames
  dummy_data <- tibble::tribble(
    ~first, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  )

  dummy_data2 <- dummy_data %>%
    add_na(to_na = "N/A") %>%
    purrr::pluck()

  # see if first column is converted to NA
  expect_true(dummy_data %>%
                add_na(to_na = "N/A") %>%
                dplyr::pull(first) %>%
                is.na() %>%
                all())

  # test for special_to_na = FALSE-- doesn't work
  expect_true(add_na(dummy_data, special_to_na = FALSE)[, 1] %>%
    stringr::str_detect("\\?") %>%
    any(na.rm = TRUE))

  # test convert_types()
  ## ERROR: Error in as_mapper(.f, ...) : argument ".f" is missing, with no default
  expect_is(convert_types(clean_rows(presidents) %>% purrr::pluck(1))$Born, "Date")

  # test remove_footnotes()
  # test that footnotes are removed
  ## ERROR: Error in colSums(!is.na(dat)) :
  # 'x' must be an array of at least two dimensions
  expect_true(all(!stringr::str_detect(remove_footnotes(college) %>% purrr::pluck(1)$Founded, "\\[")))
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


test_that("clean_rows works", {
  # Reads and cleans presidents tables with all default parameters
  presidents_raw <- read_wikitables(presidents)
  presidents_clean <- clean_rows(presidents_raw)

  # test that clean_rows() removes double header PASSES TEST
  expect_equal(
    nrow(presidents_clean[[1]]),
    nrow(presidents_raw[[1]]) - 3
  )
  expect_equal(
    nrow(presidents_clean[[2]]),
    nrow(presidents_raw[[2]]) - 1
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
})
