#' @name read_wiki_table
#' @title read_wiki_table
#' @importFrom dplyr %>%
#' @param url A character vector of the url of a wikipedia page containing a table. Default is 1.
#' @param table_number A numeric denoting which number table the desired table is on the page (e.g. the second table on the page)
#' @param remove_footnotes A boolean denoting whether footnote markings are to be removed or not. Default is TRUE.
#' @param to_na A character string that when solitary in a dataframe cell is to be converted to NA. Default is ''.
#' @param special_to_na A boolean denoting whether solitary special characters in dataframe cells are to be converted to NA. Default is TRUE.
#' @param replace_linebreak A character string denoting what linebreaks within dataframe cells are to be replaced with. Default is ' ,'.
#' @param ... passes arguments to clean_wiki_names()
#' @return Dataframe of wikipedia table
#' @examples
#' read_wiki_table('https://en.wikipedia.org/wiki/List_of_most-followed_Instagram_accounts')
#' read_wiki_table('https://en.wikipedia.org/wiki/List_of_cryptids', 3, remove_footnotes = FALSE)
#' read_wiki_table('https://en.wikipedia.org/wiki/List_of_Pixar_films')
#' @export


# You can learn more about package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some
# useful keyboard shortcuts for package authoring: Build and Reload Package: 'Ctrl + Shift + B'
# Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

read_wiki_table <- function(url, table_number = 1, replace_linebreak = ", ", remove_footnotes = TRUE, to_na = "", special_to_na = TRUE, ...) {

  wiki_table <- read_wiki_raw(url, table_number, replace_linebreak) %>%
    # removes empty columns this is often necessary for tables that have a columns of images
    clean_rows() %>%
    clean_wiki_names(...) %>%
    add_na(to_na, special_to_na) %>%
    janitor::remove_empty(which = "cols") %>%
    convert_types()

  if (remove_footnotes) {
    # remove footnotes from data
    wiki_table <- as.data.frame(purrr::map(wiki_table, ~stringr::str_remove_all(.x, "\\[.*]"))) %>%
      add_na(to_na, special_to_na) %>%
      janitor::remove_empty(which = "cols")
  }
  return(wiki_table)
}


#' @name read_all_tables
#' @title read_all_tables
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @param url A character vector of the url of a wikipedia page containing a table. Default is 1.
#' @param remove_footnotes A boolean denoting whether footnote markings are to be removed or not. Default is TRUE.
#' @param to_na A character string that when solitary in a dataframe cell is to be converted to NA. Default is ''.
#' @param special_to_na A boolean denoting whether solitary special characters in dataframe cells are to be converted to NA. Default is TRUE.
#' @param replace_linebreak A character string denoting what linebreaks within dataframe cells are to be replaced with. Default is ' ,'.
#' @param ... passes arguments to clean_wiki_names()
#' @return A list of dataframes of all tables found in wikipedia url
#' @examples
#' read_all_tables('https://en.wikipedia.org/wiki/List_of_most-followed_Instagram_accounts')
#' read_all_tables('https://en.wikipedia.org/wiki/List_of_cryptids', remove_footnotes = FALSE)
#' read_all_tables('https://en.wikipedia.org/wiki/List_of_Pixar_films')
#' @export

read_all_tables <- function(url, remove_footnotes = TRUE, to_na = "", special_to_na = TRUE, replace_linebreak = ", ", ...) {
  # read in table from webpage
  wiki_tables <- xml2::read_html(url)

  #replace line breaks in cells with comma
  comma <- rvest::xml_node(xml2::read_xml(paste("<wiki_table><span>", "</span></wiki_table>", sep = replace_linebreak)), "span")
  xml2::xml_add_sibling(rvest::xml_nodes(wiki_tables, "br"), comma)

  wiki_tables <- wiki_tables %>%
    rvest::html_nodes("table.wikitable")%>%
    map(~ purrr::pluck(.x)) %>%
    map(~ rvest::html_table(.x, fill = TRUE)) %>%
    map(~ clean_rows(.x)) %>%
    # removes empty columns
    # this is often necessary for tables that have a columns of images --- which R doesn't read in
    map(clean_wiki_names, ...) %>%
    map(~ add_na(.x, to_na, special_to_na)) %>%
    map(~ janitor::remove_empty(.x, which = "cols")) %>%
    map(~ convert_types(.x))

  if (remove_footnotes) {
    # remove footnotes from data
    wiki_tables <- map(wiki_tables, ~ as.data.frame(
      map(.x, ~ stringr::str_remove_all(.x, "\\[.*]"))))%>%
      map(~ add_na(.x))%>%
      map(~ janitor::remove_empty(.x, which = "cols"))
  }
  return(wiki_tables)
}

#' @name read_wiki_raw
#' @title read_wiki_raw
#' @importFrom dplyr %>%
#' @param url A character vector of the url of a wikipedia page containing a table. Default is 1.
#' @param table_number A numeric denoting which number table the desired table is on the page (e.g. the second table on the page)
#' @param replace_linebreak A character string denoting what linebreaks within dataframe cells are to be replaced with. Default is ' ,'.
#' @return Dataframe of wikipedia table
#' @examples
#' read_wiki_raw('https://en.wikipedia.org/wiki/List_of_most-followed_Instagram_accounts')
#' read_wiki_raw('https://en.wikipedia.org/wiki/List_of_cryptids', 3)
#' read_wiki_raw('https://en.wikipedia.org/wiki/List_of_Pixar_films')
#' @export

read_wiki_raw <- function(url, table_number = 1, replace_linebreak = ", "){
  # read in html from webpage
  wiki_table <- xml2::read_html(url)

  # replace line breaks in cells with comma
  comma <- rvest::xml_node(xml2::read_xml(paste("<wiki_table><span>", "</span></wiki_table>", sep = replace_linebreak)),
                           "span")
  xml2::xml_add_sibling(rvest::xml_nodes(wiki_table, "br"), comma)

  # extract table from html
  wiki_table <- wiki_table %>%
    rvest::html_nodes("table.wikitable") %>%
    purrr::pluck(table_number) %>%
    rvest::html_table(fill = TRUE)

  return(wiki_table)
}
