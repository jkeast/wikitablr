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
  clean_college1 <- clean_college %>% purrr::pluck(1)
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
    "_", "five", 7,
    "N/A", "ten", "eleven"
  )

  expect_false(clean_wiki_names(dummy_data_1) %>%
                 purrr::pluck(1) %>%
                 pull(first) %>%
                 stringr::str_detect("\\@") %>%
                 any())

  # test clean_rows()
  # test that duplicate of header is removed (this one has a double header)
  # ERROR: Error in as_mapper(.f, ...) : argument ".f" is missing, with no default
  presidents1 <- presidents %>% purrr::pluck(1)
  expect_lt(nrow(clean_rows(presidents1)), nrow(presidents1))

  # test that rows with same values in all column are removed
  ## read_wikitables not working for marvel page
  marvel1 <- marvel %>%  purrr::pluck(1)
  expect_lt(nrow(clean_rows(marvel1)), nrow(marvel1))

  # test add_na()
  ## I'm confused about applying these functions to the dummy data because
  ## they are supposed to take in a list of data frames
  dummy_data <- tibble::tribble(
    ~first, ~second, ~third,
    "?", "two", "three",
    "_", "five", 7,
    "N/A", "ten", "eleven"
  )

  dummy_data2 <- dummy_data %>%
    add_na(to_na = "N/A") %>%
    purrr::pluck()

  # see if first column is converted to NA
  expect_true(dummy_data %>%
                add_na(to_na = "N/A") %>%
                pull(first) %>%
                is.na() %>%
                all())

  # test for special_to_na = FALSE-- doesn't work
  expect_true(add_na(dummy_data, special_to_na = FALSE)[, 1] %>%
    str_detect("\\?") %>%
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
